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

-- Script to populate DQ SVG images into the database
--
set schema 'ores';

DO $$
declare
    v_dataset_id uuid;
begin
    -- Get the dataset ID using (name, subject_area_name, domain_name)
    select id into v_dataset_id
    from ores.dq_datasets_tbl
    where name = 'Solvaris Country Flag Images'
      and subject_area_name = 'Country Flags'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: name="Solvaris Country Flag Images", subject_area="Country Flags", domain="Reference Data"';
    end if;

    -- Clear existing images for this dataset (idempotency)
    delete from ores.dq_images_artefact_tbl
    where dataset_id = v_dataset_id;

    raise notice 'Populating images for dataset: %', 'Solvaris Country Flag Images';

    -- Insert images
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'al', 'Flag of al', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#ca8523"/>
  <rect width="640" height="160" y="160" fill="#9322d6"/>
  <rect width="640" height="160" y="320" fill="#6a1691"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ar', 'Flag of ar', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#f98b69"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#8d1760"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#c3be07"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ba', 'Flag of ba', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#0b8f81"/>
  <rect width="213.33" height="480" x="213.33" fill="#830eba"/>
  <rect width="213.34" height="480" x="426.66" fill="#a21571"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'be', 'Flag of be', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#026f7b"/>
  <rect width="213.33" height="480" x="213.33" fill="#4c95cd"/>
  <rect width="213.34" height="480" x="426.66" fill="#549d1b"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ca', 'Flag of ca', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#68e5fa"/>
  <rect width="213.33" height="480" x="213.33" fill="#a77fdf"/>
  <rect width="213.34" height="480" x="426.66" fill="#a8f4e0"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'cd', 'Flag of cd', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#d14451"/>
  <rect width="640" height="160" y="160" fill="#30d4f4"/>
  <rect width="640" height="160" y="320" fill="#91642c"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'da', 'Flag of da', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#af389e"/>
  <rect width="213.33" height="480" x="213.33" fill="#627b41"/>
  <rect width="213.34" height="480" x="426.66" fill="#cb324e"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'de', 'Flag of de', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#864261"/>
  <rect width="213.33" height="480" x="213.33" fill="#e2f62b"/>
  <rect width="213.34" height="480" x="426.66" fill="#fa2146"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'er', 'Flag of er', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#8e3c62"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#42bff5"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#63cff3"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'es', 'Flag of es', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#1f3f33"/>
  <circle cx="320" cy="240" r="80" fill="#f953bd"/>
  <rect x="280" y="160" width="80" height="160" fill="#f8ec73"/>
  <rect x="240" y="200" width="160" height="80" fill="#f8ec73"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'fe', 'Flag of fe', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#5789c0"/>
  <rect width="213.33" height="480" x="213.33" fill="#e78df6"/>
  <rect width="213.34" height="480" x="426.66" fill="#c6aacf"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'fn', 'Flag of fn', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#158413"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#8efb2d"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#b4d1b1"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ga', 'Flag of ga', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#b0b5d2"/>
  <rect width="213.33" height="480" x="213.33" fill="#c45577"/>
  <rect width="213.34" height="480" x="426.66" fill="#ae6942"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'gr', 'Flag of gr', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#c55c7e"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#bf826b"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#ace358"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'he', 'Flag of he', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#8d5239"/>
  <rect width="213.33" height="480" x="213.33" fill="#86cc71"/>
  <rect width="213.34" height="480" x="426.66" fill="#fcf093"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'hy', 'Flag of hy', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#bb8af9"/>
  <rect width="213.33" height="480" x="213.33" fill="#fe4147"/>
  <rect width="213.34" height="480" x="426.66" fill="#a5071d"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ir', 'Flag of ir', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#1625eb"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#360f61"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#ebf179"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'it', 'Flag of it', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#f87615"/>
  <rect width="640" height="160" y="160" fill="#d72a1d"/>
  <rect width="640" height="160" y="320" fill="#c9f348"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'je', 'Flag of je', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#0561ee"/>
  <rect width="213.33" height="480" x="213.33" fill="#564afd"/>
  <rect width="213.34" height="480" x="426.66" fill="#3535a1"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'jo', 'Flag of jo', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#a42b79"/>
  <circle cx="320" cy="240" r="80" fill="#4283c0"/>
  <rect x="280" y="160" width="80" height="160" fill="#9706ff"/>
  <rect x="240" y="200" width="160" height="80" fill="#9706ff"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ka', 'Flag of ka', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#473fd4"/>
  <rect width="213.33" height="480" x="213.33" fill="#0996a1"/>
  <rect width="213.34" height="480" x="426.66" fill="#d9e24e"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'kr', 'Flag of kr', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#07991e"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#77a563"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#40e2a1"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'lu', 'Flag of lu', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#fc7695"/>
  <rect width="213.33" height="480" x="213.33" fill="#24ab18"/>
  <rect width="213.34" height="480" x="426.66" fill="#19bbfb"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ly', 'Flag of ly', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#0f7b2a"/>
  <rect width="213.33" height="480" x="213.33" fill="#8208ee"/>
  <rect width="213.34" height="480" x="426.66" fill="#b46f1f"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ma', 'Flag of ma', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#2c478f"/>
  <rect width="213.33" height="480" x="213.33" fill="#7ad40a"/>
  <rect width="213.34" height="480" x="426.66" fill="#9ff2ca"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'mr', 'Flag of mr', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#f5bf20"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#3df69e"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#d5aa90"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ne', 'Flag of ne', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#fcb3e8"/>
  <rect width="213.33" height="480" x="213.33" fill="#b2a686"/>
  <rect width="213.34" height="480" x="426.66" fill="#bf1719"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'nt', 'Flag of nt', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#8ee331"/>
  <rect width="640" height="160" y="160" fill="#3f668c"/>
  <rect width="640" height="160" y="320" fill="#0e6b4f"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'or', 'Flag of or', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#c8bcbc"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#2a404b"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#d20100"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ol', 'Flag of ol', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#b2a120"/>
  <rect width="640" height="160" y="160" fill="#9c8236"/>
  <rect width="640" height="160" y="320" fill="#fb4867"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'pa', 'Flag of pa', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#3405bd"/>
  <rect width="213.33" height="480" x="213.33" fill="#307ad1"/>
  <rect width="213.34" height="480" x="426.66" fill="#6fb2a5"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'py', 'Flag of py', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#125523"/>
  <rect width="213.33" height="480" x="213.33" fill="#2d42f2"/>
  <rect width="213.34" height="480" x="426.66" fill="#d769a4"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'qu', 'Flag of qu', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#17f202"/>
  <rect width="213.33" height="480" x="213.33" fill="#73ee90"/>
  <rect width="213.34" height="480" x="426.66" fill="#09593f"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'qn', 'Flag of qn', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#f7bb91"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#3de3c7"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#6f458c"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 're', 'Flag of re', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#db8e7b"/>
  <rect width="213.33" height="480" x="213.33" fill="#86d001"/>
  <rect width="213.34" height="480" x="426.66" fill="#39905e"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ri', 'Flag of ri', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#7e10d1"/>
  <rect width="213.33" height="480" x="213.33" fill="#7043eb"/>
  <rect width="213.34" height="480" x="426.66" fill="#081f9b"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'se', 'Flag of se', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#ea8506"/>
  <rect width="213.33" height="480" x="213.33" fill="#1a2ef7"/>
  <rect width="213.34" height="480" x="426.66" fill="#b21713"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'si', 'Flag of si', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#8c9dbd"/>
  <rect width="213.33" height="480" x="213.33" fill="#f56e05"/>
  <rect width="213.34" height="480" x="426.66" fill="#09aa11"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ta', 'Flag of ta', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#d4723f"/>
  <rect width="213.33" height="480" x="213.33" fill="#47fcdf"/>
  <rect width="213.34" height="480" x="426.66" fill="#1bfd6d"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'te', 'Flag of te', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#e2bdaa"/>
  <rect width="213.33" height="480" x="213.33" fill="#9379e6"/>
  <rect width="213.34" height="480" x="426.66" fill="#e72e31"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ul', 'Flag of ul', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#37407d"/>
  <rect width="640" height="160" y="160" fill="#d8daf2"/>
  <rect width="640" height="160" y="320" fill="#929bfc"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ut', 'Flag of ut', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#703e57"/>
  <rect width="640" height="160" y="160" fill="#2995d3"/>
  <rect width="640" height="160" y="320" fill="#bf7bdc"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'va', 'Flag of va', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#ca5691"/>
  <rect width="213.33" height="480" x="213.33" fill="#fc2ee0"/>
  <rect width="213.34" height="480" x="426.66" fill="#b28ba7"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'vl', 'Flag of vl', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#9857c5"/>
  <rect width="640" height="160" y="160" fill="#747bfc"/>
  <rect width="640" height="160" y="320" fill="#e325f2"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'wi', 'Flag of wi', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#2fb635"/>
  <rect width="213.33" height="480" x="213.33" fill="#ac5d20"/>
  <rect width="213.34" height="480" x="426.66" fill="#e12ab4"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'wy', 'Flag of wy', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#fac2fa"/>
  <rect width="213.33" height="480" x="213.33" fill="#fff1c7"/>
  <rect width="213.34" height="480" x="426.66" fill="#cf93bb"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'xa', 'Flag of xa', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#a8656a"/>
  <rect width="213.33" height="480" x="213.33" fill="#9add8f"/>
  <rect width="213.34" height="480" x="426.66" fill="#ac9953"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'xe', 'Flag of xe', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#797145"/>
  <rect width="213.33" height="480" x="213.33" fill="#02c967"/>
  <rect width="213.34" height="480" x="426.66" fill="#17689e"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ys', 'Flag of ys', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#31dc60"/>
  <circle cx="320" cy="240" r="80" fill="#8ec5a1"/>
  <rect x="280" y="160" width="80" height="160" fill="#d459ed"/>
  <rect x="240" y="200" width="160" height="80" fill="#d459ed"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ze', 'Flag of ze', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#2e7847"/>
  <rect width="213.33" height="480" x="213.33" fill="#395b25"/>
  <rect width="213.34" height="480" x="426.66" fill="#b5b76b"/>
</svg>$svg$
    );
    insert into ores.dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'fa', 'Flag of fa', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#bf9399"/>
  <rect width="213.33" height="480" x="213.33" fill="#5d9999"/>
  <rect width="213.34" height="480" x="426.66" fill="#5b8f71"/>
</svg>$svg$
    );

end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Data Quality Total Images' as entity, count(*) as count
from ores.dq_images_artefact_tbl where dataset_id in (
    select id from ores.dq_datasets_tbl
    where name = 'Solvaris Country Flag Images'
      and subject_area_name = 'Country Flags'
      and domain_name = 'Reference Data'
  )
order by entity;
