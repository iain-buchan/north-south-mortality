/* nomis_2018_definitions_other_ages_browser.js – deaths at ages outside 25–49 by sex on the cause definitions of the
   2018 paper, to go with nomis_2018_definitions_browser.js (ages 25–49).

   Public NOMIS data (table NM_161_1, deaths registered, underlying cause) for 2013–2025, by sex, area and five-year
   age group: under 1, 1–4, 5–9, ... 20–24, 50–54, ... 80–84, 85 and over. The cause groups are built exactly as in
   nomis_2018_definitions_browser.js, with two additions needed outside ages 25–49:
     * suicide follows the ONS definition by age: intentional self-harm (X60–X84) from age 10 and injury or poisoning
       of undetermined intent (Y10–Y34) from age 15;
     * deaths under 28 days carry no underlying cause in NOMIS. They are in all causes and in no cause group, and are
       given as the series NEONATAL.

   Run: open https://www.nomisweb.co.uk/ in a browser, open the developer console, paste this whole file, then
        const out = await nomis2018DefsOtherAges();     // 26 requests, one at a time, a minute or two
   out.csv is ../nomis_deaths_2018_definitions_other_ages_by_sex.csv (run of 19 November 2026);
   out.sha25_49 is the SHA-256 of the ages 25–49 file rebuilt from this pull, to compare with the file from
   nomis_2018_definitions_browser.js; out.check holds accidents, alcohol-related deaths and drug poisoning on the
   definitions of the 2026 ONS extract, by year and by age group, for comparison with that extract; out.meta records
   the rows received and the deaths left out of suicide by the age rule.

   Areas: N = North (North East, North West, Yorkshire and The Humber, East Midlands, West Midlands);
          S = South including London (East, London, South East, South West). Sex: F, M. */
window.nomis2018DefsOtherAges = async function nomis2018DefsOtherAges() {
  const API = '/api/v01/dataset/';
  const GEO = { 2013265921: 'N', 2013265922: 'N', 2013265923: 'N', 2013265924: 'N', 2013265925: 'N',
                2013265926: 'X', 2013265927: 'L', 2013265928: 'X', 2013265929: 'X' };
  const YEARS = [2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025];
  const NAGE = 20;                                   // NOMIS age codes 1-20: under 1, 1-4, 5-9, ... 85-89, 90 and over
  const AGECODES = Array.from({ length: NAGE }, (_, i) => i + 1);
  const SEX = { 1: 'M', 2: 'F' };
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
  const idOf = {}; for (const w of WANT) idOf[w] = ids[w];
  idOf.NEONATAL = '98';                              // deaths under 28 days, which carry no underlying cause
  const labOf = {}; for (const [l, id] of Object.entries(idOf)) labOf[id] = l;
  const NLAB = Object.keys(idOf).length;

  // 2. deaths: one request per year and sex, regions summed to N, X (South excluding London) and L
  const raw = {}, meta = { rows: {}, not_numbers: 0 };
  for (const y of YEARS) {
    raw[y] = {};
    for (const g of [1, 2]) {
      const url = API + 'NM_161_1.data.csv?geography=' + Object.keys(GEO).join(',') + '&cause_of_death=' +
        Object.values(idOf).join(',') + '&gender=' + g + '&age=' + AGECODES.join(',') + '&measure=1&measures=20100&date=' + y +
        '&select=GEOGRAPHY,CAUSE_OF_DEATH,AGE,OBS_VALUE';
      const lines = await getLines(url, 9 * NLAB * NAGE, 'deaths ' + y + ' ' + SEX[g]);
      for (const ln of lines) {
        const f = ln.split(',').map(x => x.replace(/"/g, ''));
        const k = labOf[f[1]] + '|' + GEO[f[0]] + '|' + SEX[g], a = parseInt(f[2]) - 1, v = parseFloat(f[3]);
        if (isNaN(v) || !(a >= 0 && a < NAGE) || !GEO[f[0]] || !labOf[f[1]]) { meta.not_numbers++; continue; }
        if (!raw[y][k]) raw[y][k] = new Array(NAGE).fill(0); raw[y][k][a] += v;
      }
      meta.rows[y + SEX[g]] = lines.length;
      window.__nomisProgress = y + ' ' + SEX[g];
    }
  }

  // 3. series for one area, sex and year (20 age groups)
  const zeros = () => new Array(NAGE).fill(0);
  const add = (...vs) => vs.reduce((s, v) => s.map((x, i) => x + v[i]), zeros());
  const sub = (a, ...vs) => vs.reduce((s, v) => s.map((x, i) => x - v[i]), a.slice());
  const from = (v, first) => v.map((x, i) => (i >= first ? x : 0));        // keep age groups from index 'first' up
  const series = (area, sex, y) => {
    const L = (...labels) => add(...labels.map(l => raw[y][l + '|' + area + '|' + sex] || zeros()));
    const s = {};
    const drugAcc = L('X40', 'X41', 'X42', 'X43', 'X44');
    const Fdrug = L('F11', 'F12', 'F13', 'F14', 'F15', 'F16', 'F18', 'F19');
    const K74x = sub(L('K74'), L('K74.3', 'K74.4', 'K74.5'));
    s.ALL = L('A00-R99,U00-Y89'); s.CVD = L('I00-I99'); s.DIAB = L('E10-E14');
    s.ACC18 = sub(L('V01-X59'), drugAcc, L('X45'));                       // accidents without accidental drug and alcohol poisoning
    s.DRUG18 = add(Fdrug, drugAcc, L('X85'));                             // drug-related, 2017 extract definition
    s.SUI_ANY_AGE = L('X60-X84', 'Y10-Y34');                              // as in the ages 25-49 script
    s.SUI = add(from(L('X60-X84'), 3), from(L('Y10-Y34'), 4));            // ONS definition: X60-X84 from age 10, Y10-Y34 from age 15
    s.ALC18 = add(L('F10', 'G31.2', 'G62.1', 'I42.6', 'K29.2', 'K70', 'K73', 'K86.0', 'X45', 'Y15'), K74x);   // alcohol-related, 2017 extract definition (no X65)
    s.CANCER_XB = sub(L('C00-C97'), L('C50')); s.BREAST = L('C50'); s.CANCER = L('C00-C97');
    s.RESP = L('J00-J99');
    s.DIGEST_NONALC = sub(L('K00-K93'), L('K29.2', 'K70', 'K73', 'K86.0'), K74x);
    s.MENT_NEURO = sub(L('F00-F99', 'G00-G99'), L('F10', 'G31.2', 'G62.1'), Fdrug);
    s.COVID = L('U07.1', 'U07.2', 'U10.9');
    s.c_X40_44 = drugAcc; s.c_X45 = L('X45'); s.c_X65 = L('X65'); s.c_Y15 = L('Y15'); s.c_I426 = L('I42.6');
    s.c_DRUGSUI = L('X60', 'X61', 'X62', 'X63', 'X64', 'Y10', 'Y11', 'Y12', 'Y13', 'Y14'); s.c_E66 = L('E66');
    s.NEONATAL = L('NEONATAL');
    return s;
  };
  const D = {};                                      // D[year][area][sex][series] = deaths by age group; S = X + L
  for (const y of YEARS) { D[y] = {}; for (const a of ['N', 'X', 'L']) { D[y][a] = {}; for (const sx of ['M', 'F']) D[y][a][sx] = series(a, sx, y); }
    D[y].S = {}; for (const sx of ['M', 'F']) { D[y].S[sx] = {}; for (const k of Object.keys(D[y].X[sx])) D[y].S[sx][k] = add(D[y].X[sx][k], D[y].L[sx][k]); } }
  const sha = async (text) => Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256', new TextEncoder().encode(text)))).map(b => b.toString(16).padStart(2, '0')).join('');

  // 4. the ages 25-49 file rebuilt from this pull, as nomis_2018_definitions_browser.js writes it
  const OLD = ['ALL', 'CVD', 'DIAB', 'ACC18', 'DRUG18', 'SUI_ANY_AGE', 'ALC18', 'CANCER_XB', 'BREAST', 'RESP', 'DIGEST_NONALC', 'MENT_NEURO', 'COVID',
    'c_X40_44', 'c_X45', 'c_X65', 'c_Y15', 'c_I426', 'c_DRUGSUI', 'c_E66'];
  const old = ['series,area,sex,year,a25_29,a30_34,a35_39,a40_44,a45_49'];
  for (const k of OLD) for (const a of ['N', 'X', 'L']) for (const sx of ['F', 'M']) for (const y of YEARS)
    old.push([k === 'SUI_ANY_AGE' ? 'SUI' : k, a, sx, y, ...D[y][a][sx][k].slice(6, 11)].join(','));
  const text25_49 = old.join('\n') + '\n';

  // 5. the other ages: under 1 to 20-24 and 50-54 to 85 and over (85-89 and 90 and over together)
  const other = (v) => [...v.slice(0, 6), ...v.slice(11, 18), v[18] + v[19]];
  const NEW = ['ALL', 'CVD', 'DIAB', 'CANCER', 'ALC18', 'DRUG18', 'SUI', 'ACC18', 'NEONATAL'];
  const csv = ['series,area,sex,year,a00,a01_04,a05_09,a10_14,a15_19,a20_24,a50_54,a55_59,a60_64,a65_69,a70_74,a75_79,a80_84,a85plus'];
  for (const k of NEW) for (const a of ['N', 'S']) for (const sx of ['F', 'M']) for (const y of YEARS) csv.push([k, a, sx, y, ...other(D[y][a][sx][k])].join(','));
  const textOther = csv.join('\n') + '\n';

  // 6. accidents, alcohol-related deaths and drug poisoning on the definitions of the 2026 extract, other ages, by year and by age group
  const cur = { Accidents: s => add(s.ACC18, s.c_X40_44, s.c_X45), Alcohol: s => add(s.ALC18, s.c_X65), Drug: s => add(s.DRUG18, s.c_DRUGSUI) };
  const ck = ['cause,area,sex,by,deaths'];
  for (const c of Object.keys(cur)) for (const a of ['N', 'S']) for (const sx of ['F', 'M']) {
    const byYear = YEARS.map(y => other(cur[c](D[y][a][sx])).reduce((p, q) => p + q, 0));
    const byAge = YEARS.map(y => other(cur[c](D[y][a][sx]))).reduce((p, q) => p.map((x, i) => x + q[i]));
    ck.push([c, a, sx, 'year', ...byYear].join(',')); ck.push([c, a, sx, 'age', ...byAge].join(','));
  }
  // deaths left out of suicide by the age rule (all years, areas and sexes together)
  let young = 0; for (const y of YEARS) for (const a of ['N', 'S']) for (const sx of ['F', 'M']) young += D[y][a][sx].SUI_ANY_AGE.reduce((p, q) => p + q, 0) - D[y][a][sx].SUI.reduce((p, q) => p + q, 0);
  meta.left_out_of_suicide_by_age_rule = young; meta.labels = NLAB;
  return { meta, sha25_49: await sha(text25_49), rows25_49: old.length - 1, csv: textOther, shaOther: await sha(textOther), check: ck.join('\n') + '\n', D };
};
