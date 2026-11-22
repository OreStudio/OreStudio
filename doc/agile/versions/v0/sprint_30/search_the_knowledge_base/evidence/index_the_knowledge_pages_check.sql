\pset pager off
with pages as (
    select id, path, org,
           regexp_count(org, '^\*+ ', 1, 'n') as heading_lines
    from ores_orgmode_knowledge_pages_tbl
    where valid_to = '9999-12-31 23:59:59+00'
),
idx as (
    select page_id, count(*) as sections
    from ores_orgmode_knowledge_index_sections_tbl
    group by page_id
)
select count(*) as pages,
       count(*) filter (where i.sections is null) as pages_not_indexed,
       count(*) filter (where i.sections = p.heading_lines + 1) as exact_match,
       count(*) filter (where i.sections <> p.heading_lines + 1) as mismatch,
       sum(p.heading_lines + 1) as expected_sections,
       sum(i.sections) as indexed_sections
from pages p left join idx i on i.page_id = p.id;

select p.path, regexp_count(p.org, '^\*+ ', 1, 'n') + 1 as expected, i.sections
from ores_orgmode_knowledge_pages_tbl p
join (select page_id, count(*) sections from ores_orgmode_knowledge_index_sections_tbl group by page_id) i
  on i.page_id = p.id
where p.valid_to = '9999-12-31 23:59:59+00'
  and i.sections <> regexp_count(p.org, '^\*+ ', 1, 'n') + 1
limit 10;

select (select count(*) from ores_orgmode_knowledge_index_pages_tbl) as index_pages,
       (select count(*) from ores_orgmode_knowledge_index_sections_tbl) as index_sections,
       (select count(*) from ores_orgmode_knowledge_index_links_tbl) as index_links,
       (select count(*) from ores_orgmode_knowledge_index_sections_tbl where search is null or search = ''::tsvector) as empty_search,
       (select md5(string_agg(page_id::text || ord || anchor || heading_path || text, '|' order by page_id, ord)) from ores_orgmode_knowledge_index_sections_tbl) as sections_md5,
       (select md5(string_agg(page_id::text || section_ord || ord || target_id || text, '|' order by page_id, section_ord, ord)) from ores_orgmode_knowledge_index_links_tbl) as links_md5;
