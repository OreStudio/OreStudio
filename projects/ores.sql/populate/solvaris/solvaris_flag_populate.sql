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

DO $$
declare
    v_dataset_id uuid;
begin
    -- Get the dataset ID using (name, subject_area_name, domain_name)
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where name = 'Solvaris Country Flag Images'
      and subject_area_name = 'Country Flags'
      and domain_name = 'Reference Data'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: name="Solvaris Country Flag Images", subject_area="Country Flags", domain="Reference Data"';
    end if;

    -- Clear existing images for this dataset (idempotency)
    delete from ores_dq_images_artefact_tbl
    where dataset_id = v_dataset_id;

    raise notice 'Populating images for dataset: %', 'Solvaris Country Flag Images';

    -- Insert images
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'al', 'Flag of al', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#ca8523"/>
  <rect width="640" height="160" y="160" fill="#9322d6"/>
  <rect width="640" height="160" y="320" fill="#6a1691"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ar', 'Flag of ar', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#f98b69"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#8d1760"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#c3be07"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ba', 'Flag of ba', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#0b8f81"/>
  <rect width="213.33" height="480" x="213.33" fill="#830eba"/>
  <rect width="213.34" height="480" x="426.66" fill="#a21571"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'be', 'Flag of be', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#026f7b"/>
  <rect width="213.33" height="480" x="213.33" fill="#4c95cd"/>
  <rect width="213.34" height="480" x="426.66" fill="#549d1b"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ca', 'Flag of ca', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#68e5fa"/>
  <rect width="213.33" height="480" x="213.33" fill="#a77fdf"/>
  <rect width="213.34" height="480" x="426.66" fill="#a8f4e0"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'cd', 'Flag of cd', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#d14451"/>
  <rect width="640" height="160" y="160" fill="#30d4f4"/>
  <rect width="640" height="160" y="320" fill="#91642c"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'da', 'Flag of da', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#af389e"/>
  <rect width="213.33" height="480" x="213.33" fill="#627b41"/>
  <rect width="213.34" height="480" x="426.66" fill="#cb324e"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'de', 'Flag of de', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#864261"/>
  <rect width="213.33" height="480" x="213.33" fill="#e2f62b"/>
  <rect width="213.34" height="480" x="426.66" fill="#fa2146"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'er', 'Flag of er', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#8e3c62"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#42bff5"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#63cff3"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'es', 'Flag of es', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#1f3f33"/>
  <circle cx="320" cy="240" r="80" fill="#f953bd"/>
  <rect x="280" y="160" width="80" height="160" fill="#f8ec73"/>
  <rect x="240" y="200" width="160" height="80" fill="#f8ec73"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'fe', 'Flag of fe', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#5789c0"/>
  <rect width="213.33" height="480" x="213.33" fill="#e78df6"/>
  <rect width="213.34" height="480" x="426.66" fill="#c6aacf"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'fn', 'Flag of fn', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#158413"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#8efb2d"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#b4d1b1"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ga', 'Flag of ga', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#b0b5d2"/>
  <rect width="213.33" height="480" x="213.33" fill="#c45577"/>
  <rect width="213.34" height="480" x="426.66" fill="#ae6942"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'gr', 'Flag of gr', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#c55c7e"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#bf826b"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#ace358"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'he', 'Flag of he', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#8d5239"/>
  <rect width="213.33" height="480" x="213.33" fill="#86cc71"/>
  <rect width="213.34" height="480" x="426.66" fill="#fcf093"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'hy', 'Flag of hy', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#bb8af9"/>
  <rect width="213.33" height="480" x="213.33" fill="#fe4147"/>
  <rect width="213.34" height="480" x="426.66" fill="#a5071d"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ir', 'Flag of ir', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#1625eb"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#360f61"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#ebf179"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'it', 'Flag of it', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#f87615"/>
  <rect width="640" height="160" y="160" fill="#d72a1d"/>
  <rect width="640" height="160" y="320" fill="#c9f348"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'je', 'Flag of je', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#0561ee"/>
  <rect width="213.33" height="480" x="213.33" fill="#564afd"/>
  <rect width="213.34" height="480" x="426.66" fill="#3535a1"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'jo', 'Flag of jo', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#a42b79"/>
  <circle cx="320" cy="240" r="80" fill="#4283c0"/>
  <rect x="280" y="160" width="80" height="160" fill="#9706ff"/>
  <rect x="240" y="200" width="160" height="80" fill="#9706ff"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ka', 'Flag of ka', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#473fd4"/>
  <rect width="213.33" height="480" x="213.33" fill="#0996a1"/>
  <rect width="213.34" height="480" x="426.66" fill="#d9e24e"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'kr', 'Flag of kr', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#07991e"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#77a563"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#40e2a1"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'lu', 'Flag of lu', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#fc7695"/>
  <rect width="213.33" height="480" x="213.33" fill="#24ab18"/>
  <rect width="213.34" height="480" x="426.66" fill="#19bbfb"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ly', 'Flag of ly', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#0f7b2a"/>
  <rect width="213.33" height="480" x="213.33" fill="#8208ee"/>
  <rect width="213.34" height="480" x="426.66" fill="#b46f1f"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ma', 'Flag of ma', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#2c478f"/>
  <rect width="213.33" height="480" x="213.33" fill="#7ad40a"/>
  <rect width="213.34" height="480" x="426.66" fill="#9ff2ca"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'mr', 'Flag of mr', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#f5bf20"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#3df69e"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#d5aa90"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ne', 'Flag of ne', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#fcb3e8"/>
  <rect width="213.33" height="480" x="213.33" fill="#b2a686"/>
  <rect width="213.34" height="480" x="426.66" fill="#bf1719"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'nt', 'Flag of nt', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#8ee331"/>
  <rect width="640" height="160" y="160" fill="#3f668c"/>
  <rect width="640" height="160" y="320" fill="#0e6b4f"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'or', 'Flag of or', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#c8bcbc"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#2a404b"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#d20100"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ol', 'Flag of ol', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#b2a120"/>
  <rect width="640" height="160" y="160" fill="#9c8236"/>
  <rect width="640" height="160" y="320" fill="#fb4867"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'pa', 'Flag of pa', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#3405bd"/>
  <rect width="213.33" height="480" x="213.33" fill="#307ad1"/>
  <rect width="213.34" height="480" x="426.66" fill="#6fb2a5"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'py', 'Flag of py', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#125523"/>
  <rect width="213.33" height="480" x="213.33" fill="#2d42f2"/>
  <rect width="213.34" height="480" x="426.66" fill="#d769a4"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'qu', 'Flag of qu', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#17f202"/>
  <rect width="213.33" height="480" x="213.33" fill="#73ee90"/>
  <rect width="213.34" height="480" x="426.66" fill="#09593f"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'qn', 'Flag of qn', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#f7bb91"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#3de3c7"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#6f458c"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 're', 'Flag of re', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#db8e7b"/>
  <rect width="213.33" height="480" x="213.33" fill="#86d001"/>
  <rect width="213.34" height="480" x="426.66" fill="#39905e"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ri', 'Flag of ri', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#7e10d1"/>
  <rect width="213.33" height="480" x="213.33" fill="#7043eb"/>
  <rect width="213.34" height="480" x="426.66" fill="#081f9b"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'se', 'Flag of se', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#ea8506"/>
  <rect width="213.33" height="480" x="213.33" fill="#1a2ef7"/>
  <rect width="213.34" height="480" x="426.66" fill="#b21713"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'si', 'Flag of si', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#8c9dbd"/>
  <rect width="213.33" height="480" x="213.33" fill="#f56e05"/>
  <rect width="213.34" height="480" x="426.66" fill="#09aa11"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ta', 'Flag of ta', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#d4723f"/>
  <rect width="213.33" height="480" x="213.33" fill="#47fcdf"/>
  <rect width="213.34" height="480" x="426.66" fill="#1bfd6d"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'te', 'Flag of te', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#e2bdaa"/>
  <rect width="213.33" height="480" x="213.33" fill="#9379e6"/>
  <rect width="213.34" height="480" x="426.66" fill="#e72e31"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ul', 'Flag of ul', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#37407d"/>
  <rect width="640" height="160" y="160" fill="#d8daf2"/>
  <rect width="640" height="160" y="320" fill="#929bfc"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ut', 'Flag of ut', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#703e57"/>
  <rect width="640" height="160" y="160" fill="#2995d3"/>
  <rect width="640" height="160" y="320" fill="#bf7bdc"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'va', 'Flag of va', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#ca5691"/>
  <rect width="213.33" height="480" x="213.33" fill="#fc2ee0"/>
  <rect width="213.34" height="480" x="426.66" fill="#b28ba7"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'vl', 'Flag of vl', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#9857c5"/>
  <rect width="640" height="160" y="160" fill="#747bfc"/>
  <rect width="640" height="160" y="320" fill="#e325f2"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'wi', 'Flag of wi', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#2fb635"/>
  <rect width="213.33" height="480" x="213.33" fill="#ac5d20"/>
  <rect width="213.34" height="480" x="426.66" fill="#e12ab4"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'wy', 'Flag of wy', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#fac2fa"/>
  <rect width="213.33" height="480" x="213.33" fill="#fff1c7"/>
  <rect width="213.34" height="480" x="426.66" fill="#cf93bb"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'xa', 'Flag of xa', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#a8656a"/>
  <rect width="213.33" height="480" x="213.33" fill="#9add8f"/>
  <rect width="213.34" height="480" x="426.66" fill="#ac9953"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'xe', 'Flag of xe', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#797145"/>
  <rect width="213.33" height="480" x="213.33" fill="#02c967"/>
  <rect width="213.34" height="480" x="426.66" fill="#17689e"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ys', 'Flag of ys', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#31dc60"/>
  <circle cx="320" cy="240" r="80" fill="#8ec5a1"/>
  <rect x="280" y="160" width="80" height="160" fill="#d459ed"/>
  <rect x="240" y="200" width="160" height="80" fill="#d459ed"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ze', 'Flag of ze', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#2e7847"/>
  <rect width="213.33" height="480" x="213.33" fill="#395b25"/>
  <rect width="213.34" height="480" x="426.66" fill="#b5b76b"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ab', 'Flag of ab', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#b70202"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#58796b"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#1c1283"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ad', 'Flag of ad', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#309d56"/>
  <rect width="640" height="160" y="160" fill="#eeb447"/>
  <rect width="640" height="160" y="320" fill="#451ffe"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ag', 'Flag of ag', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#3f477b"/>
  <circle cx="320" cy="240" r="80" fill="#bb08a1"/>
  <rect x="280" y="160" width="80" height="160" fill="#7ebb27"/>
  <rect x="240" y="200" width="160" height="80" fill="#7ebb27"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ak', 'Flag of ak', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#fa4a9b"/>
  <circle cx="320" cy="240" r="80" fill="#41b70c"/>
  <rect x="280" y="160" width="80" height="160" fill="#861d43"/>
  <rect x="240" y="200" width="160" height="80" fill="#861d43"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'am', 'Flag of am', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#2eaeb5"/>
  <rect width="213.33" height="480" x="213.33" fill="#214a8b"/>
  <rect width="213.34" height="480" x="426.66" fill="#6ab070"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'an', 'Flag of an', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#f4ce3e"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#00c333"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#960d90"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ao', 'Flag of ao', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#73e31e"/>
  <circle cx="320" cy="240" r="80" fill="#a4b7b3"/>
  <rect x="280" y="160" width="80" height="160" fill="#6bcb53"/>
  <rect x="240" y="200" width="160" height="80" fill="#6bcb53"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ap', 'Flag of ap', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#1f8c32"/>
  <rect width="640" height="160" y="160" fill="#04e647"/>
  <rect width="640" height="160" y="320" fill="#695e29"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'as', 'Flag of as', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#9e8fe6"/>
  <circle cx="320" cy="240" r="80" fill="#a910fc"/>
  <rect x="280" y="160" width="80" height="160" fill="#da7569"/>
  <rect x="240" y="200" width="160" height="80" fill="#da7569"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'at', 'Flag of at', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#10d1e5"/>
  <rect width="640" height="160" y="160" fill="#930b8a"/>
  <rect width="640" height="160" y="320" fill="#afb0f9"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'av', 'Flag of av', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#b8177f"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#c42c23"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#5dc55c"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'az', 'Flag of az', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#c8940a"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#fdf8ec"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#def753"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'bi', 'Flag of bi', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#82dba4"/>
  <rect width="213.33" height="480" x="213.33" fill="#dc704f"/>
  <rect width="213.34" height="480" x="426.66" fill="#5cbf01"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'bl', 'Flag of bl', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#7fe84a"/>
  <rect width="640" height="160" y="160" fill="#19a8f0"/>
  <rect width="640" height="160" y="320" fill="#b258b2"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'bo', 'Flag of bo', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#3fb0dd"/>
  <circle cx="320" cy="240" r="80" fill="#532f0c"/>
  <rect x="280" y="160" width="80" height="160" fill="#ad6063"/>
  <rect x="240" y="200" width="160" height="80" fill="#ad6063"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'br', 'Flag of br', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#2ccde6"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#c1754d"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#c138ab"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'bu', 'Flag of bu', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#98c4c2"/>
  <rect width="213.33" height="480" x="213.33" fill="#b8cb58"/>
  <rect width="213.34" height="480" x="426.66" fill="#acfffc"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'by', 'Flag of by', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#280864"/>
  <rect width="213.33" height="480" x="213.33" fill="#a85259"/>
  <rect width="213.34" height="480" x="426.66" fill="#cea57a"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ce', 'Flag of ce', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#92eaed"/>
  <rect width="213.33" height="480" x="213.33" fill="#63c45b"/>
  <rect width="213.34" height="480" x="426.66" fill="#33cfcb"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ch', 'Flag of ch', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#97ef0c"/>
  <rect width="640" height="160" y="160" fill="#e82514"/>
  <rect width="640" height="160" y="320" fill="#ccc39e"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ci', 'Flag of ci', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#c5a9b3"/>
  <rect width="213.33" height="480" x="213.33" fill="#480590"/>
  <rect width="213.34" height="480" x="426.66" fill="#0abfe2"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'cl', 'Flag of cl', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#c60525"/>
  <rect width="640" height="160" y="160" fill="#3dd28e"/>
  <rect width="640" height="160" y="320" fill="#13efe2"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'co', 'Flag of co', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#0740f8"/>
  <circle cx="320" cy="240" r="80" fill="#88669d"/>
  <rect x="280" y="160" width="80" height="160" fill="#5ce871"/>
  <rect x="240" y="200" width="160" height="80" fill="#5ce871"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'cr', 'Flag of cr', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#a7a884"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#fcd42e"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#5c8f7b"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'cu', 'Flag of cu', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#d621c2"/>
  <rect width="213.33" height="480" x="213.33" fill="#1bd75a"/>
  <rect width="213.34" height="480" x="426.66" fill="#57b770"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'cy', 'Flag of cy', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#45495f"/>
  <rect width="213.33" height="480" x="213.33" fill="#f9c1fc"/>
  <rect width="213.34" height="480" x="426.66" fill="#c5677c"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'di', 'Flag of di', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#d2450b"/>
  <rect width="213.33" height="480" x="213.33" fill="#17e8b9"/>
  <rect width="213.34" height="480" x="426.66" fill="#3b589a"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'do', 'Flag of do', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#25d5da"/>
  <circle cx="320" cy="240" r="80" fill="#f2f4ea"/>
  <rect x="280" y="160" width="80" height="160" fill="#73b3b1"/>
  <rect x="240" y="200" width="160" height="80" fill="#73b3b1"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'dr', 'Flag of dr', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#4b62c5"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#218dc8"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#cde828"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'du', 'Flag of du', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#4eb9dd"/>
  <rect width="213.33" height="480" x="213.33" fill="#bdeb0c"/>
  <rect width="213.34" height="480" x="426.66" fill="#ea0c7a"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'dy', 'Flag of dy', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#3bf284"/>
  <rect width="213.33" height="480" x="213.33" fill="#c984f9"/>
  <rect width="213.34" height="480" x="426.66" fill="#41ac77"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'eb', 'Flag of eb', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#8926cd"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#40e0f3"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#bacb2e"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ec', 'Flag of ec', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#747315"/>
  <circle cx="320" cy="240" r="80" fill="#72ba2d"/>
  <rect x="280" y="160" width="80" height="160" fill="#05dfda"/>
  <rect x="240" y="200" width="160" height="80" fill="#05dfda"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ed', 'Flag of ed', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#7d7b39"/>
  <rect width="640" height="160" y="160" fill="#b2050c"/>
  <rect width="640" height="160" y="320" fill="#60ced0"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ef', 'Flag of ef', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#c9284f"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#ead5bc"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#add4be"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'eg', 'Flag of eg', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#58a5d2"/>
  <circle cx="320" cy="240" r="80" fill="#3da113"/>
  <rect x="280" y="160" width="80" height="160" fill="#7aee7b"/>
  <rect x="240" y="200" width="160" height="80" fill="#7aee7b"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ei', 'Flag of ei', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#8e71c7"/>
  <rect width="213.33" height="480" x="213.33" fill="#01fe8f"/>
  <rect width="213.34" height="480" x="426.66" fill="#46472c"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'el', 'Flag of el', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#dab193"/>
  <rect width="640" height="160" y="160" fill="#6ac1d2"/>
  <rect width="640" height="160" y="320" fill="#8433d1"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'em', 'Flag of em', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#9fb09b"/>
  <rect width="213.33" height="480" x="213.33" fill="#1c58dd"/>
  <rect width="213.34" height="480" x="426.66" fill="#4e2125"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'en', 'Flag of en', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#7a4be4"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#09bbab"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#b7c310"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'eo', 'Flag of eo', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#d4bbce"/>
  <circle cx="320" cy="240" r="80" fill="#36a94a"/>
  <rect x="280" y="160" width="80" height="160" fill="#603d03"/>
  <rect x="240" y="200" width="160" height="80" fill="#603d03"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ep', 'Flag of ep', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#e901d3"/>
  <rect width="640" height="160" y="160" fill="#6342ca"/>
  <rect width="640" height="160" y="320" fill="#3ce1c5"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'eq', 'Flag of eq', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#2f8604"/>
  <rect width="213.33" height="480" x="213.33" fill="#88d86e"/>
  <rect width="213.34" height="480" x="426.66" fill="#41eea8"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'et', 'Flag of et', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#d31f2c"/>
  <rect width="640" height="160" y="160" fill="#2d0f95"/>
  <rect width="640" height="160" y="320" fill="#8380b1"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'eu', 'Flag of eu', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#3051c6"/>
  <rect width="213.33" height="480" x="213.33" fill="#2055d4"/>
  <rect width="213.34" height="480" x="426.66" fill="#65acf3"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ev', 'Flag of ev', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#0311a7"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#332eb6"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#c964d3"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ex', 'Flag of ex', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="160" y="0" fill="#5cbcbc"/>
  <rect width="640" height="160" y="160" fill="#fbcae6"/>
  <rect width="640" height="160" y="320" fill="#65e0ca"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ey', 'Flag of ey', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="213.33" height="480" x="0" fill="#21d6c5"/>
  <rect width="213.33" height="480" x="213.33" fill="#a39d19"/>
  <rect width="213.34" height="480" x="426.66" fill="#fbb667"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
        dataset_id, image_id, version, key, description, svg_data
    ) values (
        v_dataset_id, gen_random_uuid(), 0, 'ez', 'Flag of ez', $svg$<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">
  <rect width="640" height="480" fill="#3b9970"/>
  <polygon points="0,0 200,0 640,480 440,480" fill="#64aee1"/>
  <polygon points="440,0 640,0 640,200 600,240 560,280 520,320 480,360 440,400 400,440 400,480 240,480 240,440 200,400 160,360 120,320 80,280 40,240 0,200 0,0" fill="#2fc1ec"/>
</svg>$svg$
    );
    insert into ores_dq_images_artefact_tbl (
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
from ores_dq_images_artefact_tbl where dataset_id in (
    select id from ores_dq_datasets_tbl
    where name = 'Solvaris Country Flag Images'
      and subject_area_name = 'Country Flags'
      and domain_name = 'Reference Data'
  )
order by entity;
