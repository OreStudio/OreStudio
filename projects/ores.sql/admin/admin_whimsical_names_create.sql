/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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

--
-- Whimsical name generation functions for tenant database naming.
-- Generates Heroku-style names like "silent_meadow" or "autumn_frost_42".
--
-- This file is part of ores_admin database utilities.
--

-- Word lists for whimsical name generation (nature-themed, Heroku-style)
create or replace function admin_whimsical_adjectives_fn()
returns text[] as $$
    select array[
        'autumn', 'hidden', 'bitter', 'misty', 'silent', 'empty', 'dry', 'dark',
        'summer', 'icy', 'delicate', 'quiet', 'white', 'cool', 'spring', 'winter',
        'patient', 'twilight', 'crimson', 'wispy', 'weathered', 'blue', 'billowing',
        'broken', 'cold', 'damp', 'falling', 'frosty', 'green', 'late', 'lingering',
        'bold', 'little', 'morning', 'muddy', 'old', 'red', 'rough', 'still',
        'small', 'sparkling', 'shy', 'wandering', 'withered', 'wild', 'black',
        'young', 'holy', 'solitary', 'fragrant', 'aged', 'snowy', 'proud', 'floral',
        'restless', 'divine', 'polished', 'ancient', 'purple', 'lively', 'nameless',
        'golden', 'silver', 'crystal', 'velvet'
    ];
$$ language sql immutable;

create or replace function admin_whimsical_nouns_fn()
returns text[] as $$
    select array[
        'waterfall', 'river', 'breeze', 'moon', 'rain', 'wind', 'sea', 'morning',
        'snow', 'lake', 'sunset', 'pine', 'shadow', 'leaf', 'dawn', 'glitter',
        'forest', 'hill', 'cloud', 'meadow', 'sun', 'glade', 'bird', 'brook',
        'butterfly', 'bush', 'dew', 'dust', 'field', 'fire', 'flower', 'firefly',
        'feather', 'grass', 'haze', 'mountain', 'night', 'pond', 'darkness',
        'snowflake', 'silence', 'sound', 'sky', 'shape', 'surf', 'thunder',
        'violet', 'water', 'wildflower', 'wave', 'resonance', 'wood', 'dream',
        'cherry', 'tree', 'fog', 'frost', 'voice', 'paper', 'frog', 'smoke',
        'star', 'horizon', 'summit', 'cascade', 'pinnacle'
    ];
$$ language sql immutable;

-- Generates a random whimsical name without any prefix or suffix.
-- Returns names like: "silent_meadow", "autumn_frost"
create or replace function admin_generate_whimsical_name_fn()
returns text as $$
declare
    adjectives text[];
    nouns text[];
    adj text;
    noun text;
begin
    adjectives := admin_whimsical_adjectives_fn();
    nouns := admin_whimsical_nouns_fn();

    adj := adjectives[1 + floor(random() * array_length(adjectives, 1))::int];
    noun := nouns[1 + floor(random() * array_length(nouns, 1))::int];

    return adj || '_' || noun;
end;
$$ language plpgsql volatile;

-- Generates a whimsical name with an optional numeric suffix.
-- Parameters:
--   with_suffix: If true, appends a random 4-digit number (e.g., "silent_meadow_4217")
-- Returns names like: "silent_meadow" or "silent_meadow_4217"
create or replace function admin_generate_whimsical_name_fn(with_suffix boolean)
returns text as $$
declare
    base_name text;
    suffix int;
begin
    base_name := admin_generate_whimsical_name_fn();

    if with_suffix then
        suffix := 1000 + floor(random() * 9000)::int;  -- 1000-9999
        return base_name || '_' || suffix::text;
    else
        return base_name;
    end if;
end;
$$ language plpgsql volatile;

-- Generates a database name with the 'ores_' prefix.
-- Parameters:
--   with_suffix: If true, appends a random 4-digit number
-- Returns names like: "ores_silent_meadow" or "ores_silent_meadow_4217"
create or replace function admin_generate_database_name_fn(with_suffix boolean default false)
returns text as $$
begin
    return 'ores_' || admin_generate_whimsical_name_fn(with_suffix);
end;
$$ language plpgsql volatile;

-- Generates a unique database name that doesn't exist in the provided list.
-- Useful when creating new tenant databases to avoid collisions.
-- Parameters:
--   existing_names: Array of already-used database names
--   max_attempts: Maximum number of generation attempts before adding suffix (default 10)
-- Returns a unique name, adding numeric suffix if needed after max_attempts
create or replace function admin_generate_unique_database_name_fn(
    existing_names text[] default array[]::text[],
    max_attempts int default 10
)
returns text as $$
declare
    candidate text;
    attempt int := 0;
begin
    -- first try without suffix
    loop
        attempt := attempt + 1;
        candidate := admin_generate_database_name_fn(false);

        if not (candidate = any(existing_names)) then
            return candidate;
        end if;

        exit when attempt >= max_attempts;
    end loop;

    -- fall back to suffix if all attempts collided
    loop
        candidate := admin_generate_database_name_fn(true);

        if not (candidate = any(existing_names)) then
            return candidate;
        end if;
    end loop;
end;
$$ language plpgsql volatile;

-- Convenience function to check all existing databases on the server.
-- Returns a unique database name that doesn't conflict with any existing database.
create or replace function admin_generate_unique_database_name_from_server_fn()
returns text as $$
declare
    existing_dbs text[];
begin
    select array_agg(datname) into existing_dbs
    from pg_database
    where datname like 'ores_%';

    return admin_generate_unique_database_name_fn(coalesce(existing_dbs, array[]::text[]));
end;
$$ language plpgsql volatile;
