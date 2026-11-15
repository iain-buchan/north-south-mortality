/* nomis_2018_definitions_browser.js – deaths at ages 25–49 by sex on the cause definitions of the 2018 paper.

   The 2018 Lancet Public Health paper used a bespoke ONS extract (2017) whose cause groups differ from those in the
   2025 and 2026 extracts: accidents exclude accidental drug and alcohol poisoning (X40–X45); drug-related deaths are
   F11–F16, F18–F19, X40–X44 and X85, with intentional and undetermined drug poisoning left in suicide; alcohol-related
   deaths exclude X65. This script rebuilds those groups from public NOMIS data (table NM_161_1, deaths registered,
   underlying cause) for 2013–2025, by sex, five-year age group 25–29 to 45–49, and area.

   Run: open https://www.nomisweb.co.uk/ in a browser, open the developer console, paste this whole file, then
        const out = await nomis2018Defs();        // 15 requests, one at a time, under a minute
   out.annual holds the deaths (CSV text, one row per series, area and sex; 65 values = 13 years x 5 age groups, year
   by year); out.check holds standardised rates worked out here as a cross-check on the analysis script; out.meta
   records the rows received per year.

   As run on 15 November 2026: 13 years x 4,950 rows, none missing. ../nomis_deaths_2018_definitions_by_sex.csv
   holds out.annual with one row per series, area, sex and year (five age columns).

   Areas: N = North (North East, North West, Yorkshire and The Humber, East Midlands, West Midlands);
          X = South excluding London (East, South East, South West); L = London. Sex: M, F. */
window.nomis2018Defs = async function nomis2018Defs() {
  const API = '/api/v01/dataset/';
  const GEO = { 2013265921: 'N', 2013265922: 'N', 2013265923: 'N', 2013265924: 'N', 2013265925: 'N',
                2013265926: 'X', 2013265927: 'L', 2013265928: 'X', 2013265929: 'X' };
  const YEARS = [2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025];
  const AGECODES = [7, 8, 9, 10, 11];                // NOMIS age codes for 25-29, 30-34, 35-39, 40-44, 45-49
  const NAGE = AGECODES.length;
  const W = [6000, 6500, 7000, 7000, 7000];          // 2013 European Standard Population weights for those ages
  const SEX = { 1: 'M', 2: 'F' };                    // NM_161_1 gender codes
  const POPSEX = { 5: 'M', 6: 'F' };                 // NM_31_1 sex codes
  const WANT = ['A00-R99,U00-Y89', 'C00-C97', 'C50', 'E10-E14', 'E66', 'F00-F99', 'G00-G99', 'I00-I99', 'J00-J99',
    'K00-K93', 'V01-X59', 'X60-X84', 'Y10-Y34', 'F10', 'G31.2', 'G62.1', 'I42.6', 'K29.2', 'K70', 'K73', 'K74',
    'K74.3', 'K74.4', 'K74.5', 'K86.0', 'X45', 'X65', 'Y15', 'F11', 'F12', 'F13', 'F14', 'F15', 'F16', 'F18', 'F19',
    'X40', 'X41', 'X42', 'X43', 'X44', 'X60', 'X61', 'X62', 'X63', 'X64', 'X85', 'Y10', 'Y11', 'Y12', 'Y13', 'Y14',
    'U07.1', 'U07.2', 'U10.9'];

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
  const labOf = {}; for (const w of WANT) labOf[ids[w]] = w;
  const NLAB = WANT.length;

  // 2. deaths: one request per year, both sexes, regions summed to N, X and L
  const raw = {}, meta = {};
  const fetchYear = async (y) => {
    const url = API + 'NM_161_1.data.csv?geography=' + Object.keys(GEO).join(',') + '&cause_of_death=' +
      WANT.map(w => ids[w]).join(',') + '&gender=1,2&age=' + AGECODES.join(',') + '&measure=1&measures=20100&date=' + y +
      '&select=GEOGRAPHY,CAUSE_OF_DEATH,GENDER,AGE,OBS_VALUE';
    const lines = await getLines(url, 9 * NLAB * NAGE * 2, 'deaths ' + y);
    let nan = 0; const agg = {};
    for (const ln of lines) {
      const f = ln.split(',').map(x => x.replace(/"/g, ''));
      const k = labOf[f[1]] + '|' + GEO[f[0]] + '|' + SEX[f[2]], a = AGECODES.indexOf(parseInt(f[3])), v = parseFloat(f[4]);
      if (isNaN(v) || a < 0) { nan++; continue; }
      if (!agg[k]) agg[k] = new Array(NAGE).fill(0); agg[k][a] += v;
    }
    raw[y] = agg; meta[y] = { rows: lines.length, nan };
  };
  for (const y of YEARS) await fetchYear(y);

  // 3. populations by sex
  const pl = await getLines(API + 'NM_31_1.data.csv?geography=' + Object.keys(GEO).join(',') + '&sex=5,6&age=' + AGECODES.join(',') +
    '&date=' + YEARS.join(',') + '&measures=20100&select=GEOGRAPHY,DATE,SEX,AGE,OBS_VALUE', 9 * YEARS.length * 2 * NAGE, 'population');
  const pop = {};
  for (const ln of pl) {
    const f = ln.split(',').map(x => x.replace(/"/g, ''));
    const k = f[1] + '|' + GEO[f[0]] + '|' + POPSEX[f[2]]; if (!pop[k]) pop[k] = new Array(NAGE).fill(0);
    pop[k][AGECODES.indexOf(parseInt(f[3]))] += parseInt(f[4]);
  }
  meta.population_rows = pl.length;

  // 4. series for one area, sex and year
  const zeros = () => new Array(NAGE).fill(0);
  const add = (...vs) => vs.reduce((s, v) => s.map((x, i) => x + v[i]), zeros());
  const sub = (a, ...vs) => vs.reduce((s, v) => s.map((x, i) => x - v[i]), a.slice());
  const series = (area, sex, y) => {
    const L = (...labels) => add(...labels.map(l => raw[y][l + '|' + area + '|' + sex] || zeros()));
    const s = {};
    const drugAcc = L('X40', 'X41', 'X42', 'X43', 'X44');
    const Fdrug = L('F11', 'F12', 'F13', 'F14', 'F15', 'F16', 'F18', 'F19');
    const K74x = sub(L('K74'), L('K74.3', 'K74.4', 'K74.5'));
    s.ALL = L('A00-R99,U00-Y89'); s.CVD = L('I00-I99'); s.DIAB = L('E10-E14');
    s.ACC18 = sub(L('V01-X59'), drugAcc, L('X45'));                       // accidents without accidental drug and alcohol poisoning
    s.DRUG18 = add(Fdrug, drugAcc, L('X85'));                             // drug-related, 2017 extract definition
    s.SUI = L('X60-X84', 'Y10-Y34');                                      // suicide, including all intentional and undetermined poisoning
    s.ALC18 = add(L('F10', 'G31.2', 'G62.1', 'I42.6', 'K29.2', 'K70', 'K73', 'K86.0', 'X45', 'Y15'), K74x);   // alcohol-related, 2017 extract definition (no X65)
    s.CANCER_XB = sub(L('C00-C97'), L('C50')); s.BREAST = L('C50');
    // groups inside 'other causes'
    s.RESP = L('J00-J99');
    s.DIGEST_NONALC = sub(L('K00-K93'), L('K29.2', 'K70', 'K73', 'K86.0'), K74x);
    s.MENT_NEURO = sub(L('F00-F99', 'G00-G99'), L('F10', 'G31.2', 'G62.1'), Fdrug);
    s.COVID = L('U07.1', 'U07.2', 'U10.9');
    // components: for moving between the 2017 and the current definitions, and the two codes that sit in two groups (I42.6, Y15)
    s.c_X40_44 = drugAcc; s.c_X45 = L('X45'); s.c_X65 = L('X65'); s.c_Y15 = L('Y15'); s.c_I426 = L('I42.6');
    s.c_DRUGSUI = L('X60', 'X61', 'X62', 'X63', 'X64', 'Y10', 'Y11', 'Y12', 'Y13', 'Y14'); s.c_E66 = L('E66');
    return s;
  };
  const D = {};                                      // D[year][area][sex][series] = deaths by age group
  for (const y of YEARS) { D[y] = {}; for (const a of ['N', 'X', 'L']) { D[y][a] = {}; for (const sx of ['M', 'F']) D[y][a][sx] = series(a, sx, y); } }
  const KEYS = Object.keys(D[YEARS[0]].N.M);

  // 5. deaths, one row per series, area and sex: 13 years x 5 age groups, year by year
  const an = ['series,area,sex,' + YEARS.map(y => ['25', '30', '35', '40', '45'].map(g => 'y' + y + '_a' + g).join(',')).join(',')];
  for (const k of KEYS) for (const a of ['N', 'X', 'L']) for (const sx of ['M', 'F']) an.push([k, a, sx, ...YEARS.map(y => D[y][a][sx][k].join(','))].join(','));

  // 6. check table: standardised rates at ages 25-49, North and South including London, by sex, for two periods
  const PER = { '2014-16': [2014, 2015, 2016], '2023-25': [2023, 2024, 2025] };
  const pooledD = (k, areas, sx, ys) => add(...ys.flatMap(y => areas.map(a => D[y][a][sx][k])));
  const pooledP = (areas, sx, ys) => add(...ys.flatMap(y => areas.map(a => pop[y + '|' + a + '|' + sx])));
  const dsr = (d, p) => { let num = 0, wsum = 0; for (let i = 0; i < NAGE; i++) { num += W[i] * d[i] / p[i]; wsum += W[i]; } return num / wsum * 1e5; };
  const ck = ['series,sex,period,dsr_N,dsr_S,gap,ratio'];
  for (const k of KEYS) for (const sx of ['F', 'M']) for (const [pn, ys] of Object.entries(PER)) {
    const n = dsr(pooledD(k, ['N'], sx, ys), pooledP(['N'], sx, ys)), s = dsr(pooledD(k, ['X', 'L'], sx, ys), pooledP(['X', 'L'], sx, ys));
    ck.push([k, sx, pn, n.toFixed(3), s.toFixed(3), (n - s).toFixed(3), s ? (n / s).toFixed(3) : 'NaN'].join(','));
  }
  return { meta, nlabels: NLAB, annual: an.join('\n'), check: ck.join('\n') };
};
