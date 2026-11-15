/* nomis_other_split_browser.js – what is inside the residual 'other causes' of the North–South mortality gap.

   Public data only: NOMIS NM_161_1 (deaths registered, by underlying cause, region, five-year age group) and
   NM_31_1 (ONS mid-year population estimates). Persons, ages under 75, years 2015–2025, pooled into 2015–19,
   2020–24 and 2021–25. Rates are directly standardised to the 2013 European Standard Population, per 100,000.

   Run: open https://www.nomisweb.co.uk/ in a browser, open the developer console, paste this whole file, then
        const out = await nomisOtherSplit();     // 13 requests, one at a time, about a minute
   out.table1 and out.table2 are the summary tables; out.long has every series, band, period and area;
   out.counts and out.pops are the pooled deaths and populations behind them (all CSV text); out.meta records
   the rows received per year. The same origin is needed because the NOMIS API does not send CORS headers.
   The pooled deaths in ../nomis_deaths_by_category_pooled.csv are out.counts from the run of 14 November 2026.

   Areas: N = North (North East, North West, Yorkshire and The Humber, East Midlands, West Midlands);
          X = South excluding London (East, South East, South West); L = London; S = X + L. */
window.nomisOtherSplit = async function nomisOtherSplit() {
  const API = '/api/v01/dataset/';
  const GEO = { 2013265921: 'N', 2013265922: 'N', 2013265923: 'N', 2013265924: 'N', 2013265925: 'N',
                2013265926: 'X', 2013265927: 'L', 2013265928: 'X', 2013265929: 'X' };
  const YEARS = [2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025];
  const PER = { '2015-19': [2015, 2016, 2017, 2018, 2019], '2020-24': [2020, 2021, 2022, 2023, 2024],
                '2021-25': [2021, 2022, 2023, 2024, 2025] };
  const NAGE = 16;                                   // NOMIS age codes 1-16 = under 1, 1-4, 5-9, ... 70-74
  const AGECODES = Array.from({ length: NAGE }, (_, i) => i + 1).join(',');
  const W = [1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000, 7000, 6500, 6000, 5500, 5000];
  const BANDS = { '25-49': [6, 7, 8, 9, 10], '<75': Array.from({ length: NAGE }, (_, i) => i) };
  const WANT = ['A00-R99,U00-Y89', 'A00-B99', 'C00-C97', 'D00-D48', 'D50-D89', 'E00-E90', 'F00-F99', 'G00-G99',
    'H00-H59', 'H60-H95', 'I00-I99', 'J00-J99', 'K00-K93', 'L00-L99', 'M00-M99', 'N00-N99', 'O00-O99', 'P00-P96',
    'Q00-Q99', 'R00-R99', 'U509,V01-Y89', 'U00-U85', 'V01-X59', 'X60-X84', 'X85-Y09', 'Y10-Y34', 'E10-E14', 'E66',
    'F10', 'G31.2', 'G62.1', 'I42.6', 'K29.2', 'K70', 'K73', 'K74', 'K74.3', 'K74.4', 'K74.5', 'K86.0', 'X45', 'X65',
    'Y15', 'F11', 'F12', 'F13', 'F14', 'F15', 'F16', 'F18', 'F19', 'X40', 'X41', 'X42', 'X43', 'X44', 'X60', 'X61',
    'X62', 'X63', 'X64', 'X85', 'Y10', 'Y11', 'Y12', 'Y13', 'Y14', 'F01', 'F03', 'G30', 'U07.1', 'U07.2', 'U10.9',
    'C33-C34', 'C18', 'C19-C21', 'C50', 'C15', 'C16', 'C22', 'C25', 'C53', 'C81-C96', 'C00-C14', 'C71', 'J40-J47',
    'J09-J18', 'K70-K77', 'G40', 'G41', 'B20-B24', 'B15-B19', 'A40', 'A41', 'R99', 'I20-I25', 'I60-I69', 'N17-N19'];

  // a request that fails, or comes back short or empty, is tried again (four attempts in all)
  const retry = async (fn, what) => {
    for (let a = 1; ; a++) {
      try { return await fn(); } catch (e) { if (a === 4) throw new Error(what + ': ' + e.message); }
      await new Promise(r => setTimeout(r, 3000 * a));
    }
  };
  const getLines = (url, expected, what) => retry(async () => {
    const lines = (await (await fetch(url)).text()).trim().split('\n').slice(1);
    if (lines.length !== expected) throw new Error(lines.length + ' rows, expected ' + expected);
    return lines;
  }, what);

  // 1. NOMIS cause codelist: the first word of each description is the ICD-10 label
  const cl = (await retry(async () => (await fetch(API + 'NM_161_1/cause_of_death.def.sdmx.json')).json(), 'codelist')).structure.codelists.codelist[0].code;
  const ids = {};
  for (const c of cl) { const tok = c.description.value.split(' ')[0]; if (!(tok in ids)) ids[tok] = String(c.value); }
  const missing = WANT.filter(w => !(w in ids));
  if (missing.length) throw new Error('labels not in the NOMIS codelist: ' + missing.join(' '));
  const idOf = {}; for (const w of WANT) idOf[w] = ids[w];
  idOf.NEONATAL = '98';                              // deaths under 28 days, which carry no underlying cause
  const labOf = {}; for (const [l, id] of Object.entries(idOf)) labOf[id] = l;
  const NLAB = Object.keys(idOf).length;             // 99

  // 2. deaths: one request per year, regions summed to N, X and L
  const raw = {}, meta = {};
  const fetchYear = async (y) => {
    const url = API + 'NM_161_1.data.csv?geography=' + Object.keys(GEO).join(',') + '&cause_of_death=' +
      Object.values(idOf).join(',') + '&gender=0&age=' + AGECODES + '&measure=1&measures=20100&date=' + y +
      '&select=GEOGRAPHY,CAUSE_OF_DEATH,AGE,OBS_VALUE';
    const lines = await getLines(url, 9 * NLAB * NAGE, 'deaths ' + y);
    let nan = 0; const agg = {};
    for (const ln of lines) {
      const f = ln.split(',').map(x => x.replace(/"/g, ''));
      const area = GEO[f[0]], l = labOf[f[1]], a = parseInt(f[2]) - 1, v = parseFloat(f[3]);
      if (isNaN(v)) { nan++; continue; }
      const k = l + '|' + area; if (!agg[k]) agg[k] = new Array(NAGE).fill(0); agg[k][a] += v;
    }
    raw[y] = agg; meta[y] = { rows: lines.length, nan };
  };
  for (const y of YEARS) await fetchYear(y);         // one at a time: requests sent in parallel are sometimes answered empty

  // 3. populations, persons
  const pl = await getLines(API + 'NM_31_1.data.csv?geography=' + Object.keys(GEO).join(',') + '&sex=7&age=' + AGECODES +
    '&date=' + YEARS.join(',') + '&measures=20100&select=GEOGRAPHY,DATE,AGE,OBS_VALUE', 9 * YEARS.length * NAGE, 'population');
  const pop = {};
  for (const ln of pl) {
    const f = ln.split(',').map(x => x.replace(/"/g, ''));
    const k = f[1] + '|' + GEO[f[0]]; if (!pop[k]) pop[k] = new Array(NAGE).fill(0);
    pop[k][parseInt(f[2]) - 1] += parseInt(f[3]);
  }
  meta.population_rows = pl.length;

  // 4. pooled deaths by series for one area and period
  const zeros = () => new Array(NAGE).fill(0);
  const add = (...vs) => vs.reduce((s, v) => s.map((x, i) => x + v[i]), zeros());
  const sub = (a, ...vs) => vs.reduce((s, v) => s.map((x, i) => x - v[i]), a.slice());
  const pooled = (label, area, years) => {
    const out = zeros();
    for (const y of years) { const v = raw[y][label + '|' + area]; if (v) for (let i = 0; i < NAGE; i++) out[i] += v[i]; }
    return out;
  };
  const popOf = (area, years) => add(...years.map(y => pop[y + '|' + area]));
  const series = (area, years) => {
    const L = (...labels) => add(...labels.map(l => pooled(l, area, years)));
    const s = {};
    s.ALL = L('A00-R99,U00-Y89'); s.CVD = L('I00-I99'); s.DIAB = L('E10-E14'); s.ACC = L('V01-X59');
    // suicide as ONS defines it: intentional self-harm from age 10, undetermined intent from age 15
    s.SUI = add(L('X60-X84').map((v, i) => (i >= 3 ? v : 0)), L('Y10-Y34').map((v, i) => (i >= 4 ? v : 0)));
    const K74x = sub(L('K74'), L('K74.3', 'K74.4', 'K74.5'));
    s.ALC_EXCL = add(L('F10', 'G31.2', 'G62.1', 'K29.2', 'K70', 'K73', 'K86.0'), K74x);   // alcohol codes outside the other named causes
    s.ALC_OVL = L('I42.6', 'X45', 'X65', 'Y15');                                          // alcohol codes inside cardiovascular, accidents, suicide
    s.ALC_FULL = add(s.ALC_EXCL, s.ALC_OVL);
    const Fdrug = L('F11', 'F12', 'F13', 'F14', 'F15', 'F16', 'F18', 'F19');
    s.DRUG_MEMO = add(Fdrug, L('X40', 'X41', 'X42', 'X43', 'X44', 'X60', 'X61', 'X62', 'X63', 'X64', 'X85', 'Y10', 'Y11', 'Y12', 'Y13', 'Y14'));
    s.OTHER_DEF = sub(s.ALL, s.CVD, s.DIAB, s.ACC, s.SUI, s.ALC_FULL);                    // the residual as defined for the ONS extract
    // mutually exclusive categories that, with CVD, DIAB, ACC, SUI and ALC_EXCL, add up to ALL
    s.CANCER = L('C00-C97'); s.NEO_OTH = L('D00-D48'); s.RESP = L('J00-J99');
    s.DEMENTIA = L('F01', 'F03', 'G30'); s.COVID = L('U07.1', 'U07.2', 'U10.9');
    s.LIVER_NONALC = sub(L('K70-K77'), L('K70', 'K73'), K74x);
    s.DIGEST_OTH = sub(L('K00-K93'), L('K70-K77', 'K29.2', 'K86.0'));
    s.MENT_DRUG = Fdrug;
    s.MENT_OTH = sub(L('F00-F99'), L('F01', 'F03', 'F10'), Fdrug);
    s.NERVOUS = sub(L('G00-G99'), L('G30', 'G31.2', 'G62.1'));
    s.INFECT = L('A00-B99'); s.ENDO_NONDIAB = sub(L('E00-E90'), s.DIAB); s.BLOOD = L('D50-D89'); s.GU = L('N00-N99');
    s.MSK_SKIN = L('M00-M99', 'L00-L99'); s.PERI_CONG_PREG = L('P00-P96', 'Q00-Q99', 'O00-O99'); s.ILLDEF = L('R00-R99');
    s.EXT_OTH = sub(L('U509,V01-Y89'), s.ACC, s.SUI); s.U_OTH = sub(L('U00-U85'), s.COVID);
    s.EYE_EAR = L('H00-H59', 'H60-H95'); s.NEONATAL = L('NEONATAL');
    s.CHECK = sub(s.ALL, ...EXCL.map(k => s[k]));                                         // must be zero
    // the five groups proposed for the ONS request, and what is left
    s.g_CANCER = s.CANCER; s.g_RESP = s.RESP; s.g_DIGEST_NONALC = add(s.LIVER_NONALC, s.DIGEST_OTH);
    s.g_MENT_NEURO = add(s.DEMENTIA, s.MENT_DRUG, s.MENT_OTH, s.NERVOUS); s.g_COVID = s.COVID;
    s.g_FIVE = add(s.g_CANCER, s.g_RESP, s.g_DIGEST_NONALC, s.g_MENT_NEURO, s.g_COVID);
    s.g_REST = sub(s.OTHER_DEF, s.g_FIVE);
    // memo items
    s.m_LUNG = L('C33-C34'); s.m_COLORECTAL = L('C18', 'C19-C21'); s.m_BREAST = L('C50'); s.m_UPPER_GI = L('C15', 'C16');
    s.m_LIVER_CA = L('C22'); s.m_PANCREAS = L('C25'); s.m_CERVIX = L('C53'); s.m_HAEM = L('C81-C96');
    s.m_HEADNECK = L('C00-C14'); s.m_BRAIN = L('C71'); s.m_COPD_ETC = L('J40-J47'); s.m_FLU_PNEUM = L('J09-J18');
    s.m_EPILEPSY = L('G40', 'G41'); s.m_HIV = L('B20-B24'); s.m_VIRAL_HEP = L('B15-B19'); s.m_SEPSIS = L('A40', 'A41');
    s.m_R99 = L('R99'); s.m_OBESITY_E66 = L('E66'); s.m_RENAL_FAIL = L('N17-N19'); s.m_IHD = L('I20-I25');
    s.m_STROKE = L('I60-I69'); s.m_ASSAULT = L('X85-Y09'); s.m_ACC_DRUG_POIS = L('X40', 'X41', 'X42', 'X43', 'X44');
    s.m_K70_ALD = L('K70');
    return s;
  };
  const EXCL = ['CVD', 'DIAB', 'ACC', 'SUI', 'ALC_EXCL', 'CANCER', 'NEO_OTH', 'RESP', 'DEMENTIA', 'COVID', 'LIVER_NONALC',
    'DIGEST_OTH', 'MENT_DRUG', 'MENT_OTH', 'NERVOUS', 'INFECT', 'ENDO_NONDIAB', 'BLOOD', 'GU', 'MSK_SKIN',
    'PERI_CONG_PREG', 'ILLDEF', 'EXT_OTH', 'U_OTH', 'EYE_EAR', 'NEONATAL'];

  // 5. standardised rates
  const dsr = (d, p, idx) => { let num = 0, wsum = 0; for (const i of idx) { num += W[i] * d[i] / p[i]; wsum += W[i]; } return num / wsum * 1e5; };
  const D = {}, P = {};                              // D[period][area][series] = deaths by age; P[period][area] = population by age
  for (const [pn, ys] of Object.entries(PER)) {
    D[pn] = { N: series('N', ys), X: series('X', ys), L: series('L', ys) }; D[pn].S = {};
    for (const k of Object.keys(D[pn].N)) D[pn].S[k] = add(D[pn].X[k], D[pn].L[k]);
    P[pn] = { N: popOf('N', ys), X: popOf('X', ys), L: popOf('L', ys) }; P[pn].S = add(P[pn].X, P[pn].L);
  }
  const KEYS = Object.keys(D['2021-25'].N);
  const rate = (k, area, pn, band) => dsr(D[pn][area][k], P[pn][area], BANDS[band]);
  const r2 = (v) => (isNaN(v) ? 'NaN' : String(+v.toFixed(2)));
  const sumBand = (v, band) => BANDS[band].reduce((s, i) => s + v[i], 0);

  // table1: one row per series; gaps by band, comparison and period, northern deaths a year, ratios
  const P1 = '2015-19', P2 = '2020-24', P3 = '2021-25';
  const gap = (k, a, b, pn, band) => rate(k, a, pn, band) - rate(k, b, pn, band);
  const t1 = ['series,g2549_NS_P1,g2549_NS_P2,g2549_NS_P3,g2549_NX_P1,g2549_NX_P3,g2549_LX_P3,g75_NS_P1,g75_NS_P2,g75_NS_P3,g75_NX_P3,g75_LX_P3,Ndpy2549_P3,Ndpy75_P3,rr2549_NS_P3,rr75_NS_P3'];
  for (const k of KEYS.filter(k => !k.startsWith('g_'))) {
    t1.push([k, r2(gap(k, 'N', 'S', P1, '25-49')), r2(gap(k, 'N', 'S', P2, '25-49')), r2(gap(k, 'N', 'S', P3, '25-49')),
      r2(gap(k, 'N', 'X', P1, '25-49')), r2(gap(k, 'N', 'X', P3, '25-49')), r2(gap(k, 'L', 'X', P3, '25-49')),
      r2(gap(k, 'N', 'S', P1, '<75')), r2(gap(k, 'N', 'S', P2, '<75')), r2(gap(k, 'N', 'S', P3, '<75')),
      r2(gap(k, 'N', 'X', P3, '<75')), r2(gap(k, 'L', 'X', P3, '<75')),
      Math.round(sumBand(D[P3].N[k], '25-49') / 5), Math.round(sumBand(D[P3].N[k], '<75') / 5),
      r2(rate(k, 'N', P3, '25-49') / rate(k, 'S', P3, '25-49')), r2(rate(k, 'N', P3, '<75') / rate(k, 'S', P3, '<75'))].join(','));
  }
  // table2: the five groups, what is left, the residual and all causes
  const t2 = ['band,period,group,dsrN,dsrS,dsrX,dsrL,gapNS,rrNS,gapNX,rrNX,Ndeaths_per_yr'];
  // long: every series, band, period and area
  const lg = ['series,band,period,dsr_N,dsr_S,dsr_X,dsr_L,deaths_per_year_N'];
  for (const band of Object.keys(BANDS)) for (const pn of Object.keys(PER)) {
    for (const k of ['g_CANCER', 'g_RESP', 'g_DIGEST_NONALC', 'g_MENT_NEURO', 'g_COVID', 'g_FIVE', 'g_REST', 'OTHER_DEF', 'ALL']) {
      const [n, s, x, l] = ['N', 'S', 'X', 'L'].map(a => rate(k, a, pn, band));
      t2.push([band, pn, k, r2(n), r2(s), r2(x), r2(l), r2(n - s), r2(n / s), r2(n - x), r2(n / x), Math.round(sumBand(D[pn].N[k], band) / 5)].join(','));
    }
    for (const k of KEYS) lg.push([k, band, pn, ...['N', 'S', 'X', 'L'].map(a => rate(k, a, pn, band).toFixed(4)), (sumBand(D[pn].N[k], band) / 5).toFixed(1)].join(','));
  }
  // pooled deaths behind the tables: main series for all periods, memo items for 2021-25 only
  const ages = ['a00', 'a01_04', 'a05_09', 'a10_14', 'a15_19', 'a20_24', 'a25_29', 'a30_34', 'a35_39', 'a40_44', 'a45_49', 'a50_54', 'a55_59', 'a60_64', 'a65_69', 'a70_74'];
  const cn = ['period,series,area,' + ages.join(',')], pp = ['period,area,' + ages.join(',')];
  const MAIN = ['ALL', 'CVD', 'DIAB', 'ACC', 'SUI', 'ALC_EXCL', 'ALC_OVL', 'DRUG_MEMO'].concat(EXCL.slice(5));
  for (const pn of Object.keys(PER)) for (const a of ['N', 'X', 'L']) {
    pp.push([pn, a, ...P[pn][a]].join(','));
    for (const k of MAIN.concat(pn === P3 ? KEYS.filter(k => k.startsWith('m_')) : [])) cn.push([pn, k, a, ...D[pn][a][k]].join(','));
  }
  return { meta, ids: idOf, table1: t1.join('\n'), table2: t2.join('\n'), long: lg.join('\n'), counts: cn.join('\n'), pops: pp.join('\n') };
};
