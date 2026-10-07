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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_outcome_functions_create.mustache
 * To modify, update the template and regenerate.
 *
 * Outcome functions
 *
 * One function per store outcome composes the sentence that outcome reports,
 * and ores_outcome_raise_fn maps an outcome code to the SQLSTATE the store
 * raises for it. A trigger names the outcome and the field; it carries neither
 * the wording nor the SQLSTATE, so both exist once, in the outcome catalogue.
 */

-- Fills a message template's {name} placeholders with the matching values. The
-- one renderer, shared by every outcome function below and mirrored by
-- ores::database::domain::describe in C++.
create or replace function ores_outcome_fill_fn(
    p_template text,
    p_names text[],
    p_values text[]
) returns text language plpgsql immutable as $$
declare
    v_text text := p_template;
    i integer;
begin
    for i in 1 .. coalesce(array_length(p_names, 1), 0) loop
        v_text := replace(v_text, '{' || p_names[i] || '}', coalesce(p_values[i], ''));
    end loop;
    return v_text;
end;
$$;

-- already_exists: A create states that no row exists, and the store holds one already. The store also raises this when a write replaces a row with the version-replace signal switched off.
create or replace function ores_outcome_already_exists_fn(
    p_entity text,
    p_field text,
    p_value text
) returns text language sql immutable as $$
    select ores_outcome_fill_fn(
        'The {entity} with {field} ''{value}'' already exists. State the version you read to replace it, or ask for a version replace.',
        array['entity','field','value']::text[],
        array[coalesce(p_entity, ''),coalesce(p_field, ''),coalesce(p_value, '')]::text[]);
$$;

-- version_conflict: A write states the version it read, and the row has moved on since. This is the optimistic-concurrency refusal. The write states the version it believes and the store states the version it holds.
create or replace function ores_outcome_version_conflict_fn(
    p_entity text,
    p_field text,
    p_value text,
    p_expected text,
    p_current text
) returns text language sql immutable as $$
    select ores_outcome_fill_fn(
        'The {entity} with {field} ''{value}'' is at version {current}, and this write states version {expected}.',
        array['entity','field','value','expected','current']::text[],
        array[coalesce(p_entity, ''),coalesce(p_field, ''),coalesce(p_value, ''),coalesce(p_expected, ''),coalesce(p_current, '')]::text[]);
$$;

-- missing_field: A validation function receives a null or an empty value where its entity requires one. The store refuses rather than storing a row that names nothing.
create or replace function ores_outcome_missing_field_fn(
    p_entity text
) returns text language sql immutable as $$
    select ores_outcome_fill_fn(
        'Invalid {entity}: value cannot be null or empty.',
        array['entity']::text[],
        array[coalesce(p_entity, '')]::text[]);
$$;

-- Raises the SQLSTATE the outcome catalogue binds to p_code, with the sentence
-- that outcome's own function composes. A trigger calls this and nothing else.
create or replace function ores_outcome_raise_fn(
    p_code text,
    p_entity text default null,
    p_field text default null,
    p_value text default null,
    p_expected text default null,
    p_current text default null
) returns void language plpgsql as $$
begin
    case p_code
    when 'already_exists' then
        raise exception '%', ores_outcome_already_exists_fn(p_entity,p_field,p_value)
            using errcode = '23505';
    when 'version_conflict' then
        raise exception '%', ores_outcome_version_conflict_fn(p_entity,p_field,p_value,p_expected,p_current)
            using errcode = 'P0002';
    when 'missing_field' then
        raise exception '%', ores_outcome_missing_field_fn(p_entity)
            using errcode = '23502';
    else
        raise exception 'Unknown outcome code: %', p_code using errcode = 'XX000';
    end case;
end;
$$;
