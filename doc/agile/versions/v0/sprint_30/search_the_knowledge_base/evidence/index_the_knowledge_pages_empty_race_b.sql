\pset pager off
select pg_sleep(1);
select *, clock_timestamp() from ores_orgmode_replace_knowledge_index_fn('{"pages":[{"page_id":"p1","path":"a.org","title":"Alpha","description":"d","tags":[]}],"sections":[{"page_id":"p1","ord":0,"anchor":"","heading_path":"","level":0,"text":"d"}],"links":[]}'::jsonb);
