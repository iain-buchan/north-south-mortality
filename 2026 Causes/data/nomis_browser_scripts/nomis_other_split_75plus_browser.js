/* nomis_other_split_75plus_browser.js – what is inside the residual 'other causes' at ages 75 and over.

   The companion of nomis_other_split_browser.js, which stops at age 74: the same NOMIS labels, the same series and the
   same pooling, for the age groups 75-79, 80-84, 85-89 and 90 and over. Public data only: NOMIS NM_161_1 (deaths
   registered, by underlying cause, region and five-year age group). Persons, years 2015-2025, pooled into 2015-19,
   2020-24 and 2021-25. Populations are not fetched here: the analysis takes them from ../nomis_pop_nsl_1981_2025.csv.

   Run: open https://www.nomisweb.co.uk/ in a browser, open the developer console, paste this whole file, then
        const out = await nomisOtherSplit75();     // 11 requests, one at a time, about half a minute
   out.counts is the CSV text of pooled deaths (main series for all periods, memo items for 2021-25 only); out.sha is
   its SHA-256; out.check is the largest absolute difference between all causes and the sum of the mutually exclusive
   categories in any cell (it must be 0); out.meta records the rows received per year. The same origin is needed because
   the NOMIS API does not send CORS headers.
   The pooled deaths in ../nomis_deaths_by_category_pooled_75plus.csv are out.counts from the run of 19 November 2026.

   Areas: N = North (North East, North West, Yorkshire and The Humber, East Midlands, West Midlands);
          X = South excluding London (East, South East, South West); L = London; S = X + L. */
window.nomisOtherSplit75 = async function nomisOtherSplit75() {
  const API = '/api/v01/dataset/';
  const GEO = { 2013265921: 'N', 2013265922: 'N', 2013265923: 'N', 2013265924: 'N', 2013265925: 'N',
                2013265926: 'X', 2013265927: 'L', 2013265928: 'X', 2013265929: 'X' };
  const YEARS = [2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025];
  const PER = { '2015-19': [2015, 2016, 2017, 2018, 2019], '2020-24': [2020, 2021, 2022, 2023, 2024],
                '2021-25': [2021, 2022, 2023, 2024, 2025] };
  const CODES = [17, 18, 19, 20];                    // NOMIS age codes 17-20 = 75-79, 80-84, 85-89, 90 and over
  const NAGE = CODES.length;
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
  idOf.NEONATAL = '98';                              // deaths under 28 days: none at these ages, kept so that the series match the under-75 file
  const labOf = {}; for (const [l, id] of Object.entries(idOf)) labOf[id] = l;
  const NLAB = Object.keys(idOf).length;             // 99

  // 2. deaths: one request per year, regions summed to N, X and L
  const raw = {}, meta = {};
  const fetchYear = async (y) => {
    const url = API + 'NM_161_1.data.csv?geography=' + Object.keys(GEO).join(',') + '&cause_of_death=' +
      Object.values(idOf).join(',') + '&gender=0&age=' + CODES.join(',') + '&measure=1&measures=20100&date=' + y +
      '&select=GEOGRAPHY,CAUSE_OF_DEATH,AGE,OBS_VALUE';
    const lines = await getLines(url, 9 * NLAB * NAGE, 'deaths ' + y);
    let nan = 0; const agg = {};
    for (const ln of lines) {
      const f = ln.split(',').map(x => x.replace(/"/g, ''));
      const area = GEO[f[0]], l = labOf[f[1]], a = CODES.indexOf(parseInt(f[2])), v = parseFloat(f[3]);
      if (isNaN(v) || a < 0 || !area || !l) { nan++; continue; }
      const k = l + '|' + area; if (!agg[k]) agg[k] = new Array(NAGE).fill(0); agg[k][a] += v;
    }
    raw[y] = agg; meta[y] = { rows: lines.length, nan };
  };
  for (const y of YEARS) await fetchYear(y);         // one at a time: requests sent in parallel are sometimes answered empty

  // 3. pooled deaths by series for one area and period (the series of nomis_other_split_browser.js)
  const zeros = () => new Array(NAGE).fill(0);
  const add = (...vs) => vs.reduce((s, v) => s.map((x, i) => x + v[i]), zeros());
  const sub = (a, ...vs) => vs.reduce((s, v) => s.map((x, i) => x - v[i]), a.slice());
  const pooled = (label, area, years) => {
    const out = zeros();
    for (const y of years) { const v = raw[y][label + '|' + area]; if (v) for (let i = 0; i < NAGE; i++) out[i] += v[i]; }
    return out;
  };
  const EXCL = ['CVD', 'DIAB', 'ACC', 'SUI', 'ALC_EXCL', 'CANCER', 'NEO_OTH', 'RESP', 'DEMENTIA', 'COVID', 'LIVER_NONALC',
    'DIGEST_OTH', 'MENT_DRUG', 'MENT_OTH', 'NERVOUS', 'INFECT', 'ENDO_NONDIAB', 'BLOOD', 'GU', 'MSK_SKIN',
    'PERI_CONG_PREG', 'ILLDEF', 'EXT_OTH', 'U_OTH', 'EYE_EAR', 'NEONATAL'];
  const series = (area, years) => {
    const L = (...labels) => add(...labels.map(l => pooled(l, area, years)));
    const s = {};
    s.ALL = L('A00-R99,U00-Y89'); s.CVD = L('I00-I99'); s.DIAB = L('E10-E14'); s.ACC = L('V01-X59');
    s.SUI = L('X60-X84', 'Y10-Y34');                 // suicide as ONS defines it; its age limits (10 and 15) do not bind at these ages
    const K74x = sub(L('K74'), L('K74.3', 'K74.4', 'K74.5'));
    s.ALC_EXCL = add(L('F10', 'G31.2', 'G62.1', 'K29.2', 'K70', 'K73', 'K86.0'), K74x);   // alcohol codes outside the other named causes
    s.ALC_OVL = L('I42.6', 'X45', 'X65', 'Y15');                                          // alcohol codes inside cardiovascular, accidents, suicide
    const Fdrug = L('F11', 'F12', 'F13', 'F14', 'F15', 'F16', 'F18', 'F19');
    s.DRUG_MEMO = add(Fdrug, L('X40', 'X41', 'X42', 'X43', 'X44', 'X60', 'X61', 'X62', 'X63', 'X64', 'X85', 'Y10', 'Y11', 'Y12', 'Y13', 'Y14'));
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

  // 4. the CSV: main series for all periods, memo items for 2021-25 only, as in the under-75 file
  const ages = ['a75_79', 'a80_84', 'a85_89', 'a90plus'];
  const cn = ['period,series,area,' + ages.join(',')];
  const MAIN = ['ALL', 'CVD', 'DIAB', 'ACC', 'SUI', 'ALC_EXCL', 'ALC_OVL', 'DRUG_MEMO'].concat(EXCL.slice(5));
  let check = 0, negative = 0;
  for (const pn of Object.keys(PER)) for (const a of ['N', 'X', 'L']) {
    const s = series(a, PER[pn]);
    for (const v of s.CHECK) check = Math.max(check, Math.abs(v));
    const keys = MAIN.concat(pn === '2021-25' ? Object.keys(s).filter(k => k.startsWith('m_')) : []);
    for (const k of keys) { for (const v of s[k]) if (v < 0) negative++; cn.push([pn, k, a, ...s[k]].join(',')); }
  }
  const counts = cn.join('\n');
  const hash = await crypto.subtle.digest('SHA-256', new TextEncoder().encode(counts));
  const sha = Array.from(new Uint8Array(hash)).map(b => b.toString(16).padStart(2, '0')).join('');
  return { meta, nlab: NLAB, check, negative, rows: cn.length - 1, chars: counts.length, sha, counts };
};
