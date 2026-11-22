/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * pgTAP tests for the function that replaces the knowledge index whole.
 *
 * The service hands it the index it built from the pages, and it is the index
 * tables' only writer.
 * - It reports how many pages, sections and links it wrote
 * - A section matches a search for its page's title
 * - The page title ranks above the section's own text
 * - A second call leaves only the second index's rows
 * - A page the second index drops leaves no section or link behind
 * - An empty index empties the tables
 *
 * Run with: pg_prove -d <database> test/orgmode_replace_knowledge_index_test.sql
 */

begin;

select plan(8);

select results_eq(
    $$select pages, sections, links from ores_orgmode_replace_knowledge_index_fn('{
        "pages": [
            {"page_id": "p1", "path": "a.org", "title": "Swaption", "description": "An option on a swap.", "tags": ["product"]},
            {"page_id": "p2", "path": "b.org", "title": "Netting", "description": "Offsetting exposures.", "tags": []}],
        "sections": [
            {"page_id": "p1", "ord": 0, "anchor": "", "heading_path": "", "level": 0, "text": "An option on a swap."},
            {"page_id": "p1", "ord": 1, "anchor": "exercise", "heading_path": "Exercise", "level": 1, "text": "Bermudan dates."},
            {"page_id": "p2", "ord": 0, "anchor": "", "heading_path": "", "level": 0, "text": "Offsetting exposures."}],
        "links": [
            {"page_id": "p1", "section_ord": 1, "ord": 0, "target_id": "p2", "text": "Netting"}]
    }'::jsonb)$$,
    $$values (2::bigint, 3::bigint, 1::bigint)$$,
    'it reports how many pages, sections and links it wrote');

select is(
    (select count(*) from ores_orgmode_knowledge_index_sections_tbl s
     where s.page_id = 'p1' and s.ord = 1
       and s.search @@ to_tsquery('english', 'swaption')),
    1::bigint,
    'a section matches a search for its page''s title');

select is(
    (select ts_rank(s.search, to_tsquery('english', 'swaption'))
          > ts_rank(s.search, to_tsquery('english', 'bermudan'))
     from ores_orgmode_knowledge_index_sections_tbl s
     where s.page_id = 'p1' and s.ord = 1),
    true,
    'the page title ranks above the section''s own text');

select results_eq(
    $$select pages, sections, links from ores_orgmode_replace_knowledge_index_fn('{
        "pages": [
            {"page_id": "p2", "path": "b.org", "title": "Netting", "description": "Offsetting exposures.", "tags": []}],
        "sections": [
            {"page_id": "p2", "ord": 0, "anchor": "", "heading_path": "", "level": 0, "text": "Offsetting exposures."}],
        "links": []
    }'::jsonb)$$,
    $$values (1::bigint, 1::bigint, 0::bigint)$$,
    'a second call reports the second index');

select is(
    (select array_agg(page_id order by page_id) from ores_orgmode_knowledge_index_pages_tbl),
    array['p2'],
    'a second call leaves only the second index''s pages');

select is(
    (select count(*) from ores_orgmode_knowledge_index_sections_tbl where page_id = 'p1')
  + (select count(*) from ores_orgmode_knowledge_index_links_tbl where page_id = 'p1'),
    0::bigint,
    'a dropped page leaves no section or link behind');

select results_eq(
    $$select pages, sections, links from ores_orgmode_replace_knowledge_index_fn('{}'::jsonb)$$,
    $$values (0::bigint, 0::bigint, 0::bigint)$$,
    'an empty index writes nothing');

select is(
    (select count(*) from ores_orgmode_knowledge_index_pages_tbl)
  + (select count(*) from ores_orgmode_knowledge_index_sections_tbl)
  + (select count(*) from ores_orgmode_knowledge_index_links_tbl),
    0::bigint,
    'an empty index empties the tables');

select * from finish();
rollback;
