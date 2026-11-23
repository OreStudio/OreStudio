\pset pager off
select pg_sleep(1);
select r.*, clock_timestamp() from ores_orgmode_replace_knowledge_index_fn((
  select jsonb_build_object(
    'pages', (select jsonb_agg(jsonb_build_object('page_id', page_id, 'path', path, 'title', title, 'description', description, 'tags', to_jsonb(tags))) from ores_orgmode_knowledge_index_pages_tbl),
    'sections', (select jsonb_agg(jsonb_build_object('page_id', page_id, 'ord', ord, 'anchor', anchor, 'heading_path', heading_path, 'level', level, 'text', text)) from ores_orgmode_knowledge_index_sections_tbl),
    'links', (select jsonb_agg(jsonb_build_object('page_id', page_id, 'section_ord', section_ord, 'ord', ord, 'target_id', target_id, 'text', text)) from ores_orgmode_knowledge_index_links_tbl)))) r;
