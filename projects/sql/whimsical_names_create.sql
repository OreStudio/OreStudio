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

SET search_path TO ores;

-- Word lists for whimsical name generation (nature-themed, Heroku-style)
CREATE OR REPLACE FUNCTION ores.whimsical_adjectives()
RETURNS TEXT[] AS $$
    SELECT ARRAY[
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
$$ LANGUAGE sql IMMUTABLE;

CREATE OR REPLACE FUNCTION ores.whimsical_nouns()
RETURNS TEXT[] AS $$
    SELECT ARRAY[
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
$$ LANGUAGE sql IMMUTABLE;

-- Generates a random whimsical name without any prefix or suffix.
-- Returns names like: "silent_meadow", "autumn_frost"
CREATE OR REPLACE FUNCTION ores.generate_whimsical_name()
RETURNS TEXT AS $$
DECLARE
    adjectives TEXT[];
    nouns TEXT[];
    adj TEXT;
    noun TEXT;
BEGIN
    adjectives := ores.whimsical_adjectives();
    nouns := ores.whimsical_nouns();

    adj := adjectives[1 + floor(random() * array_length(adjectives, 1))::int];
    noun := nouns[1 + floor(random() * array_length(nouns, 1))::int];

    RETURN adj || '_' || noun;
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Generates a whimsical name with an optional numeric suffix.
-- Parameters:
--   with_suffix: If true, appends a random 4-digit number (e.g., "silent_meadow_4217")
-- Returns names like: "silent_meadow" or "silent_meadow_4217"
CREATE OR REPLACE FUNCTION ores.generate_whimsical_name(with_suffix BOOLEAN)
RETURNS TEXT AS $$
DECLARE
    base_name TEXT;
    suffix INT;
BEGIN
    base_name := ores.generate_whimsical_name();

    IF with_suffix THEN
        suffix := 1000 + floor(random() * 9000)::int;  -- 1000-9999
        RETURN base_name || '_' || suffix::text;
    ELSE
        RETURN base_name;
    END IF;
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Generates a database name with the 'ores_' prefix.
-- Parameters:
--   with_suffix: If true, appends a random 4-digit number
-- Returns names like: "ores_silent_meadow" or "ores_silent_meadow_4217"
CREATE OR REPLACE FUNCTION ores.generate_database_name(with_suffix BOOLEAN DEFAULT FALSE)
RETURNS TEXT AS $$
BEGIN
    RETURN 'ores_' || ores.generate_whimsical_name(with_suffix);
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Generates a unique database name that doesn't exist in the provided list.
-- Useful when creating new tenant databases to avoid collisions.
-- Parameters:
--   existing_names: Array of already-used database names
--   max_attempts: Maximum number of generation attempts before adding suffix (default 10)
-- Returns a unique name, adding numeric suffix if needed after max_attempts
CREATE OR REPLACE FUNCTION ores.generate_unique_database_name(
    existing_names TEXT[] DEFAULT ARRAY[]::TEXT[],
    max_attempts INT DEFAULT 10
)
RETURNS TEXT AS $$
DECLARE
    candidate TEXT;
    attempt INT := 0;
BEGIN
    -- First try without suffix
    LOOP
        attempt := attempt + 1;
        candidate := ores.generate_database_name(false);

        IF NOT (candidate = ANY(existing_names)) THEN
            RETURN candidate;
        END IF;

        EXIT WHEN attempt >= max_attempts;
    END LOOP;

    -- Fall back to suffix if all attempts collided
    LOOP
        candidate := ores.generate_database_name(true);

        IF NOT (candidate = ANY(existing_names)) THEN
            RETURN candidate;
        END IF;
    END LOOP;
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Convenience function to check all existing databases on the server.
-- Returns a unique database name that doesn't conflict with any existing database.
CREATE OR REPLACE FUNCTION ores.generate_unique_database_name_from_server()
RETURNS TEXT AS $$
DECLARE
    existing_dbs TEXT[];
BEGIN
    SELECT array_agg(datname) INTO existing_dbs
    FROM pg_database
    WHERE datname LIKE 'ores_%';

    RETURN ores.generate_unique_database_name(COALESCE(existing_dbs, ARRAY[]::TEXT[]));
END;
$$ LANGUAGE plpgsql VOLATILE;
