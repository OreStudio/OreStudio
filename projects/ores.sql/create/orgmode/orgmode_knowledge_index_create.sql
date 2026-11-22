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

-- =============================================================================
-- Knowledge Index
-- =============================================================================
-- What ores.orgmode.service derives from the knowledge pages: one row per
-- page, one per section and one per link between pages. The index is derived
-- data, so it has no versions and no history: the service rebuilds it whole
-- from the pages, through ores_orgmode_replace_knowledge_index_fn, its only
-- writer. It describes the system tenant's pages, which every tenant reads, so
-- it has no tenant either; the knowledge search reads it for every tenant.

create table if not exists "ores_orgmode_knowledge_index_pages_tbl" (
    "page_id" text not null,
    "path" text not null,
    "title" text not null,
    "description" text not null,
    "tags" text[] not null,
    primary key (page_id),
    check ("page_id" <> '')
);

-- A section is a heading's own text, or, at order 0, the text above a page's
-- first heading. The heading path is the titles from the page's top heading
-- down to this one, joined by " / ".
create table if not exists "ores_orgmode_knowledge_index_sections_tbl" (
    "page_id" text not null references ores_orgmode_knowledge_index_pages_tbl (page_id) on delete cascade,
    "ord" integer not null,
    "anchor" text not null,
    "heading_path" text not null,
    "level" integer not null,
    "text" text not null,
    "search" tsvector not null,
    primary key (page_id, ord),
    unique (page_id, anchor),
    check ("ord" >= 0),
    check ("level" >= 0)
);

create index if not exists ores_orgmode_knowledge_index_sections_search_idx
on "ores_orgmode_knowledge_index_sections_tbl" using gin ("search");

create table if not exists "ores_orgmode_knowledge_index_links_tbl" (
    "page_id" text not null,
    "section_ord" integer not null,
    "ord" integer not null,
    "target_id" text not null,
    "text" text not null,
    primary key (page_id, section_ord, ord),
    foreign key (page_id, section_ord)
        references ores_orgmode_knowledge_index_sections_tbl (page_id, ord) on delete cascade
);

create index if not exists ores_orgmode_knowledge_index_links_target_idx
on "ores_orgmode_knowledge_index_links_tbl" ("target_id");

-- Replaces the whole index with the one given, in the caller's transaction, so
-- a reader sees the old index or the new one and never a mix. p_index holds
-- three arrays: pages, sections and links, each element an object whose keys
-- are the columns above. The search vector is weighted here, so its recipe
-- lives in one place: the page title A, the heading path B, the text D.
create or replace function ores_orgmode_replace_knowledge_index_fn(p_index jsonb)
returns table (
    pages bigint,
    sections bigint,
    links bigint
) as $$
declare
    v_pages bigint;
    v_sections bigint;
    v_links bigint;
begin
    delete from ores_orgmode_knowledge_index_links_tbl;
    delete from ores_orgmode_knowledge_index_sections_tbl;
    delete from ores_orgmode_knowledge_index_pages_tbl;

    insert into ores_orgmode_knowledge_index_pages_tbl
        (page_id, path, title, description, tags)
    select p.page_id, p.path, p.title, p.description,
           coalesce(array(select jsonb_array_elements_text(p.tags)), '{}')
    from jsonb_to_recordset(coalesce(p_index->'pages', '[]'::jsonb)) as p(
        page_id text, path text, title text, description text, tags jsonb);
    get diagnostics v_pages = row_count;

    insert into ores_orgmode_knowledge_index_sections_tbl
        (page_id, ord, anchor, heading_path, level, text, search)
    select s.page_id, s.ord, s.anchor, s.heading_path, s.level, s.text,
           setweight(to_tsvector('english', pg.title), 'A') ||
           setweight(to_tsvector('english', s.heading_path), 'B') ||
           setweight(to_tsvector('english', s.text), 'D')
    from jsonb_to_recordset(coalesce(p_index->'sections', '[]'::jsonb)) as s(
        page_id text, ord integer, anchor text, heading_path text, level integer, text text)
    join ores_orgmode_knowledge_index_pages_tbl pg on pg.page_id = s.page_id;
    get diagnostics v_sections = row_count;

    insert into ores_orgmode_knowledge_index_links_tbl
        (page_id, section_ord, ord, target_id, text)
    select l.page_id, l.section_ord, l.ord, l.target_id, l.text
    from jsonb_to_recordset(coalesce(p_index->'links', '[]'::jsonb)) as l(
        page_id text, section_ord integer, ord integer, target_id text, text text);
    get diagnostics v_links = row_count;

    return query select v_pages, v_sections, v_links;
end;
$$ language plpgsql set search_path = public, pg_temp;

comment on function ores_orgmode_replace_knowledge_index_fn(jsonb) is
'Replaces the whole knowledge index with the one given, in one transaction, so
 a reader sees the old index or the new one. The index tables'' only writer.';
