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
 * Data Quality Solvaris Countries Artefact Population Script
 *
 * Populates the dq_countries_artefact_tbl with Solvaris country data.
 * Source: country_currency.json model
 *
 * This script is idempotent.
 */

set schema 'ores';

DO $$
declare
    v_dataset_id uuid;
    v_flags_dataset_id uuid;
    v_placeholder_image_id uuid;
    v_count integer := 0;
begin
    -- Get the countries dataset ID from the datasets model
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where name = 'Solvaris Countries'
      and subject_area_name = 'Countries'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found for countries';
    end if;

    -- Get the flags dataset ID (for linking images)
    select id into v_flags_dataset_id
    from ores.dq_datasets_tbl
    where name = 'Solvaris Country Flag Images'
      and subject_area_name = 'Country Flags'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_flags_dataset_id is null then
        raise exception 'Dataset not found for flag images';
    end if;

    -- Get the placeholder image (xx.svg = "no flag available")
    select image_id into v_placeholder_image_id
    from ores.dq_images_artefact_tbl
    where dataset_id = v_flags_dataset_id
      and key = 'xx';

    if v_placeholder_image_id is null then
        raise warning 'Placeholder image (xx) not found - countries without flags will have NULL image_id';
    end if;

    -- Clear existing countries for this dataset (idempotency)
    delete from ores.dq_countries_artefact_tbl
    where dataset_id = v_dataset_id;

    raise notice 'Populating countries for dataset: %', (select name from ores.dq_datasets_tbl where id = v_dataset_id);

    -- Insert Solvaris countries with flag image links
    insert into ores.dq_countries_artefact_tbl (
        dataset_id, alpha2_code, version, alpha3_code, numeric_code, name, official_name, image_id
    )
    select
        v_dataset_id,
        c.alpha2_code,
        0,
        c.alpha3_code,
        c.numeric_code::text,
        c.name,
        c.official_name,
        coalesce(i.image_id, v_placeholder_image_id)
    from (values
        ('AL', 'XAL', 1141, 'Aerilon', 'Republic of Aerilon'),
        ('AR', 'XAR', 1147, 'Arcturia', 'Republic of Arcturia'),
        ('BA', 'XBA', 1131, 'Balthoria', 'Republic of Balthoria'),
        ('BE', 'XBE', 1135, 'Belloria', 'Republic of Belloria'),
        ('CA', 'XCA', 1132, 'Calandria', 'Republic of Calandria'),
        ('CD', 'XCD', 1135, 'Caledonia', 'Republic of Caledonia'),
        ('DA', 'XDA', 1133, 'Daeloria', 'Republic of Daeloria'),
        ('DE', 'XDE', 1137, 'Delvadia', 'Republic of Delvadia'),
        ('ER', 'XER', 1151, 'Eriador', 'Republic of Eriador'),
        ('ES', 'XES', 1152, 'Esteria', 'Republic of Esteria'),
        ('FE', 'XFE', 1139, 'Feloria', 'Republic of Feloria'),
        ('FN', 'XFN', 1148, 'Fendaria', 'Republic of Fendaria'),
        ('GA', 'XGA', 1136, 'Galdoria', 'Republic of Galdoria'),
        ('GR', 'XGR', 1153, 'Grendoria', 'Republic of Grendoria'),
        ('HE', 'XHE', 1141, 'Helvetia', 'Republic of Helvetia'),
        ('HY', 'XHY', 1161, 'Hydronia', 'Republic of Hydronia'),
        ('IR', 'XIR', 1155, 'Iridia', 'Republic of Iridia'),
        ('IT', 'XIT', 1157, 'Ithaca', 'Republic of Ithaca'),
        ('JE', 'XJE', 1143, 'Jethro', 'Republic of Jethro'),
        ('JO', 'XJO', 1153, 'Jorvik', 'Republic of Jorvik'),
        ('KA', 'XKA', 1140, 'Kaelor', 'Republic of Kaelor'),
        ('KR', 'XKR', 1157, 'Krynn', 'Republic of Krynn'),
        ('LU', 'XLU', 1161, 'Luminia', 'Republic of Luminia'),
        ('LY', 'XLY', 1165, 'Lysandria', 'Republic of Lysandria'),
        ('MA', 'XMA', 1142, 'Maldoria', 'Republic of Maldoria'),
        ('MR', 'XMR', 1159, 'Mariposa', 'Republic of Mariposa'),
        ('NE', 'XNE', 1147, 'Nektonia', 'Republic of Nektonia'),
        ('NT', 'XNT', 1162, 'Netharia', 'Republic of Netharia'),
        ('OR', 'XOR', 1161, 'Orinoco', 'Republic of Orinoco'),
        ('OL', 'XOL', 1155, 'Orlanthia', 'Republic of Orlanthia'),
        ('PA', 'XPA', 1145, 'Paldoria', 'Republic of Paldoria'),
        ('PY', 'XPY', 1169, 'Pyrrhia', 'Republic of Pyrrhia'),
        ('QU', 'XQU', 1166, 'Quentaria', 'Republic of Quentaria'),
        ('QN', 'XQN', 1159, 'Quinaria', 'Republic of Quinaria'),
        ('RE', 'XRE', 1151, 'Rendellia', 'Republic of Rendellia'),
        ('RI', 'XRI', 1155, 'Rivenia', 'Republic of Rivenia'),
        ('SE', 'XSE', 1152, 'Serendia', 'Republic of Serendia'),
        ('SI', 'XSI', 1156, 'Sildoria', 'Republic of Sildoria'),
        ('TA', 'XTA', 1149, 'Tandor', 'Republic of Tandor'),
        ('TE', 'XTE', 1153, 'Tenebria', 'Republic of Tenebria'),
        ('UL', 'XUL', 1161, 'Uldoria', 'Republic of Uldoria'),
        ('UT', 'XUT', 1169, 'Utopia', 'Republic of Utopia'),
        ('VA', 'XVA', 1151, 'Valoria', 'Republic of Valoria'),
        ('VL', 'XVL', 1162, 'Valtaria', 'Republic of Valtaria'),
        ('WI', 'XWI', 1160, 'Wintervale', 'Republic of Wintervale'),
        ('WY', 'XWY', 1176, 'Wysteria', 'Republic of Wysteria'),
        ('XA', 'XXA', 1153, 'Xandria', 'Republic of Xandria'),
        ('XE', 'XXE', 1157, 'Xenoria', 'Republic of Xenoria'),
        ('YS', 'XYS', 1172, 'Yslandia', 'Republic of Yslandia'),
        ('ZE', 'XZE', 1159, 'Zephyria', 'Republic of Zephyria'),
        ('FA', 'XFA', 1135, 'Faeland', 'Republic of Faeland')
    ) as c(alpha2_code, alpha3_code, numeric_code, name, official_name)
    left join ores.dq_images_artefact_tbl i
        on i.dataset_id = v_flags_dataset_id
        and i.key = lower(c.alpha2_code);

    get diagnostics v_count = row_count;

    raise notice 'Successfully populated % countries for dataset: %', v_count, (select name from ores.dq_datasets_tbl where id = v_dataset_id);
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Solvaris Countries Summary ---'

select 'Total Solvaris Countries' as metric, count(*) as count
from ores.dq_countries_artefact_tbl c
join ores.dq_datasets_tbl d on c.dataset_id = d.id
where d.subject_area_name = 'Countries'
  and d.domain_name = 'Reference Data'
  and d.valid_to = ores.utility_infinity_timestamp_fn();
