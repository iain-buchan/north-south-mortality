/* nomis_suicide_wales_check_browser.js – suicides at ages 25–49 among residents of England and of Wales, 2013–2016.

   Why: the 2017 ONS extract behind the 2018 paper has more suicides in the South than the 2026 extract in every
   year to 2014 (the North is identical). This pulls the public NOMIS figures (table NM_161_1, deaths registered,
   underlying cause X60–X84 and Y10–Y34) for England, Wales, and England and Wales together, by sex, to see
   whether deaths of residents of Wales account for the surplus.

   Run: open https://www.nomisweb.co.uk/ in a browser, open the developer console, paste this whole file, then
        JSON.stringify(await window.suicideEW())
   As run on 15 November 2026: 60 rows for each of the four years. The result is in
   ../nomis_suicide_england_wales_2013_16.csv, one row per year, area and sex. */
window.suicideEW = async function suicideEW() {
  const API = '/api/v01/dataset/';
  const cl = (await (await fetch(API + 'NM_161_1/cause_of_death.def.sdmx.json')).json()).structure.codelists.codelist[0].code;
  const ids = {}; for (const c of cl) { const tok = c.description.value.split(' ')[0]; if (!(tok in ids)) ids[tok] = String(c.value); }
  const causes = [ids['X60-X84'], ids['Y10-Y34']];
  const GEO = { 2092957699: 'England', 2092957700: 'Wales', 2092957703: 'EnglandAndWales' };
  const out = {}; const meta = {};
  for (const y of [2013, 2014, 2015, 2016]) {
    const url = API + 'NM_161_1.data.csv?geography=' + Object.keys(GEO).join(',') + '&cause_of_death=' + causes.join(',') +
      '&gender=1,2&age=7,8,9,10,11&measure=1&measures=20100&date=' + y + '&select=GEOGRAPHY,GENDER,AGE,CAUSE_OF_DEATH,OBS_VALUE';
    let lines = [];
    for (let a = 0; a < 4 && lines.length !== 60; a++) { lines = (await (await fetch(url)).text()).trim().split('\n').slice(1); if (lines.length !== 60) await new Promise(r => setTimeout(r, 2500)); }
    meta[y] = lines.length;
    for (const ln of lines) {
      const f = ln.split(',').map(x => x.replace(/"/g, ''));
      const k = y + '|' + GEO[f[0]] + '|' + (f[1] === '1' ? 'M' : 'F');
      out[k] = (out[k] || 0) + parseFloat(f[4]);
    }
  }
  return { marker: 'SUICIDE_EW_CHECK_V1', causes, meta, out };
};
