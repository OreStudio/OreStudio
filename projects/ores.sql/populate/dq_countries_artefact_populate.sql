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

/**
 * Data Quality Countries Artefact Population Script
 *
 * Populates the dq_countries_artefact_tbl with ISO 3166-1 country data.
 * Links countries to their flag images from the flags dataset.
 * This script is idempotent.
 */

set schema 'ores';

DO $$
declare
    v_countries_dataset_id uuid;
    v_flags_dataset_id uuid;
    v_placeholder_image_id uuid;
    v_count integer := 0;
begin
    -- Get the countries dataset ID
    select id into v_countries_dataset_id
    from ores.dq_datasets_tbl
    where name = 'ISO 3166 Countries from Wikipedia'
      and subject_area_name = 'Countries'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_countries_dataset_id is null then
        raise exception 'Dataset not found: ISO 3166 Countries from Wikipedia';
    end if;

    -- Get the flags dataset ID (for linking images)
    select id into v_flags_dataset_id
    from ores.dq_datasets_tbl
    where name = 'Country Flags from lipis/flag-icons'
      and subject_area_name = 'Countries'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_flags_dataset_id is null then
        raise exception 'Dataset not found: Country Flags from lipis/flag-icons';
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
    where dataset_id = v_countries_dataset_id;

    raise notice 'Populating countries for dataset: ISO 3166 Countries from Wikipedia';

    -- Insert countries with flag image links
    -- The flag images have keys matching lowercase alpha2 codes (e.g., 'us', 'gb')
    -- Countries without matching flags fall back to the placeholder (xx.svg)
    insert into ores.dq_countries_artefact_tbl (
        dataset_id, alpha2_code, version, alpha3_code, numeric_code, name, official_name, image_id
    )
    select
        v_countries_dataset_id,
        c.alpha2_code,
        0,
        c.alpha3_code,
        c.numeric_code,
        c.name,
        c.official_name,
        coalesce(i.image_id, v_placeholder_image_id)
    from (values
        -- A
        ('AD', 'AND', '020', 'Andorra', 'Principality of Andorra'),
        ('AE', 'ARE', '784', 'United Arab Emirates', 'United Arab Emirates'),
        ('AF', 'AFG', '004', 'Afghanistan', 'Islamic Republic of Afghanistan'),
        ('AG', 'ATG', '028', 'Antigua and Barbuda', 'Antigua and Barbuda'),
        ('AI', 'AIA', '660', 'Anguilla', 'Anguilla'),
        ('AL', 'ALB', '008', 'Albania', 'Republic of Albania'),
        ('AM', 'ARM', '051', 'Armenia', 'Republic of Armenia'),
        ('AO', 'AGO', '024', 'Angola', 'Republic of Angola'),
        ('AQ', 'ATA', '010', 'Antarctica', 'Antarctica'),
        ('AR', 'ARG', '032', 'Argentina', 'Argentine Republic'),
        ('AS', 'ASM', '016', 'American Samoa', 'American Samoa'),
        ('AT', 'AUT', '040', 'Austria', 'Republic of Austria'),
        ('AU', 'AUS', '036', 'Australia', 'Commonwealth of Australia'),
        ('AW', 'ABW', '533', 'Aruba', 'Aruba'),
        ('AX', 'ALA', '248', 'Åland Islands', 'Åland Islands'),
        ('AZ', 'AZE', '031', 'Azerbaijan', 'Republic of Azerbaijan'),
        -- B
        ('BA', 'BIH', '070', 'Bosnia and Herzegovina', 'Bosnia and Herzegovina'),
        ('BB', 'BRB', '052', 'Barbados', 'Barbados'),
        ('BD', 'BGD', '050', 'Bangladesh', 'People''s Republic of Bangladesh'),
        ('BE', 'BEL', '056', 'Belgium', 'Kingdom of Belgium'),
        ('BF', 'BFA', '854', 'Burkina Faso', 'Burkina Faso'),
        ('BG', 'BGR', '100', 'Bulgaria', 'Republic of Bulgaria'),
        ('BH', 'BHR', '048', 'Bahrain', 'Kingdom of Bahrain'),
        ('BI', 'BDI', '108', 'Burundi', 'Republic of Burundi'),
        ('BJ', 'BEN', '204', 'Benin', 'Republic of Benin'),
        ('BL', 'BLM', '652', 'Saint Barthélemy', 'Saint Barthélemy'),
        ('BM', 'BMU', '060', 'Bermuda', 'Bermuda'),
        ('BN', 'BRN', '096', 'Brunei', 'Brunei Darussalam'),
        ('BO', 'BOL', '068', 'Bolivia', 'Plurinational State of Bolivia'),
        ('BQ', 'BES', '535', 'Bonaire, Sint Eustatius and Saba', 'Bonaire, Sint Eustatius and Saba'),
        ('BR', 'BRA', '076', 'Brazil', 'Federative Republic of Brazil'),
        ('BS', 'BHS', '044', 'Bahamas', 'Commonwealth of The Bahamas'),
        ('BT', 'BTN', '064', 'Bhutan', 'Kingdom of Bhutan'),
        ('BV', 'BVT', '074', 'Bouvet Island', 'Bouvet Island'),
        ('BW', 'BWA', '072', 'Botswana', 'Republic of Botswana'),
        ('BY', 'BLR', '112', 'Belarus', 'Republic of Belarus'),
        ('BZ', 'BLZ', '084', 'Belize', 'Belize'),
        -- C
        ('CA', 'CAN', '124', 'Canada', 'Canada'),
        ('CC', 'CCK', '166', 'Cocos (Keeling) Islands', 'Cocos (Keeling) Islands'),
        ('CD', 'COD', '180', 'DR Congo', 'Democratic Republic of the Congo'),
        ('CF', 'CAF', '140', 'Central African Republic', 'Central African Republic'),
        ('CG', 'COG', '178', 'Congo', 'Republic of the Congo'),
        ('CH', 'CHE', '756', 'Switzerland', 'Swiss Confederation'),
        ('CI', 'CIV', '384', 'Côte d''Ivoire', 'Republic of Côte d''Ivoire'),
        ('CK', 'COK', '184', 'Cook Islands', 'Cook Islands'),
        ('CL', 'CHL', '152', 'Chile', 'Republic of Chile'),
        ('CM', 'CMR', '120', 'Cameroon', 'Republic of Cameroon'),
        ('CN', 'CHN', '156', 'China', 'People''s Republic of China'),
        ('CO', 'COL', '170', 'Colombia', 'Republic of Colombia'),
        ('CR', 'CRI', '188', 'Costa Rica', 'Republic of Costa Rica'),
        ('CU', 'CUB', '192', 'Cuba', 'Republic of Cuba'),
        ('CV', 'CPV', '132', 'Cape Verde', 'Republic of Cabo Verde'),
        ('CW', 'CUW', '531', 'Curaçao', 'Curaçao'),
        ('CX', 'CXR', '162', 'Christmas Island', 'Christmas Island'),
        ('CY', 'CYP', '196', 'Cyprus', 'Republic of Cyprus'),
        ('CZ', 'CZE', '203', 'Czechia', 'Czech Republic'),
        -- D
        ('DE', 'DEU', '276', 'Germany', 'Federal Republic of Germany'),
        ('DJ', 'DJI', '262', 'Djibouti', 'Republic of Djibouti'),
        ('DK', 'DNK', '208', 'Denmark', 'Kingdom of Denmark'),
        ('DM', 'DMA', '212', 'Dominica', 'Commonwealth of Dominica'),
        ('DO', 'DOM', '214', 'Dominican Republic', 'Dominican Republic'),
        ('DZ', 'DZA', '012', 'Algeria', 'People''s Democratic Republic of Algeria'),
        -- E
        ('EC', 'ECU', '218', 'Ecuador', 'Republic of Ecuador'),
        ('EE', 'EST', '233', 'Estonia', 'Republic of Estonia'),
        ('EG', 'EGY', '818', 'Egypt', 'Arab Republic of Egypt'),
        ('EH', 'ESH', '732', 'Western Sahara', 'Western Sahara'),
        ('ER', 'ERI', '232', 'Eritrea', 'State of Eritrea'),
        ('ES', 'ESP', '724', 'Spain', 'Kingdom of Spain'),
        ('ET', 'ETH', '231', 'Ethiopia', 'Federal Democratic Republic of Ethiopia'),
        -- F
        ('FI', 'FIN', '246', 'Finland', 'Republic of Finland'),
        ('FJ', 'FJI', '242', 'Fiji', 'Republic of Fiji'),
        ('FK', 'FLK', '238', 'Falkland Islands', 'Falkland Islands'),
        ('FM', 'FSM', '583', 'Micronesia', 'Federated States of Micronesia'),
        ('FO', 'FRO', '234', 'Faroe Islands', 'Faroe Islands'),
        ('FR', 'FRA', '250', 'France', 'French Republic'),
        -- G
        ('GA', 'GAB', '266', 'Gabon', 'Gabonese Republic'),
        ('GB', 'GBR', '826', 'United Kingdom', 'United Kingdom of Great Britain and Northern Ireland'),
        ('GD', 'GRD', '308', 'Grenada', 'Grenada'),
        ('GE', 'GEO', '268', 'Georgia', 'Georgia'),
        ('GF', 'GUF', '254', 'French Guiana', 'French Guiana'),
        ('GG', 'GGY', '831', 'Guernsey', 'Bailiwick of Guernsey'),
        ('GH', 'GHA', '288', 'Ghana', 'Republic of Ghana'),
        ('GI', 'GIB', '292', 'Gibraltar', 'Gibraltar'),
        ('GL', 'GRL', '304', 'Greenland', 'Greenland'),
        ('GM', 'GMB', '270', 'Gambia', 'Republic of The Gambia'),
        ('GN', 'GIN', '324', 'Guinea', 'Republic of Guinea'),
        ('GP', 'GLP', '312', 'Guadeloupe', 'Guadeloupe'),
        ('GQ', 'GNQ', '226', 'Equatorial Guinea', 'Republic of Equatorial Guinea'),
        ('GR', 'GRC', '300', 'Greece', 'Hellenic Republic'),
        ('GS', 'SGS', '239', 'South Georgia', 'South Georgia and the South Sandwich Islands'),
        ('GT', 'GTM', '320', 'Guatemala', 'Republic of Guatemala'),
        ('GU', 'GUM', '316', 'Guam', 'Guam'),
        ('GW', 'GNB', '624', 'Guinea-Bissau', 'Republic of Guinea-Bissau'),
        ('GY', 'GUY', '328', 'Guyana', 'Co-operative Republic of Guyana'),
        -- H
        ('HK', 'HKG', '344', 'Hong Kong', 'Hong Kong Special Administrative Region of the People''s Republic of China'),
        ('HM', 'HMD', '334', 'Heard Island and McDonald Islands', 'Heard Island and McDonald Islands'),
        ('HN', 'HND', '340', 'Honduras', 'Republic of Honduras'),
        ('HR', 'HRV', '191', 'Croatia', 'Republic of Croatia'),
        ('HT', 'HTI', '332', 'Haiti', 'Republic of Haiti'),
        ('HU', 'HUN', '348', 'Hungary', 'Hungary'),
        -- I
        ('ID', 'IDN', '360', 'Indonesia', 'Republic of Indonesia'),
        ('IE', 'IRL', '372', 'Ireland', 'Ireland'),
        ('IL', 'ISR', '376', 'Israel', 'State of Israel'),
        ('IM', 'IMN', '833', 'Isle of Man', 'Isle of Man'),
        ('IN', 'IND', '356', 'India', 'Republic of India'),
        ('IO', 'IOT', '086', 'British Indian Ocean Territory', 'British Indian Ocean Territory'),
        ('IQ', 'IRQ', '368', 'Iraq', 'Republic of Iraq'),
        ('IR', 'IRN', '364', 'Iran', 'Islamic Republic of Iran'),
        ('IS', 'ISL', '352', 'Iceland', 'Iceland'),
        ('IT', 'ITA', '380', 'Italy', 'Italian Republic'),
        -- J
        ('JE', 'JEY', '832', 'Jersey', 'Bailiwick of Jersey'),
        ('JM', 'JAM', '388', 'Jamaica', 'Jamaica'),
        ('JO', 'JOR', '400', 'Jordan', 'Hashemite Kingdom of Jordan'),
        ('JP', 'JPN', '392', 'Japan', 'Japan'),
        -- K
        ('KE', 'KEN', '404', 'Kenya', 'Republic of Kenya'),
        ('KG', 'KGZ', '417', 'Kyrgyzstan', 'Kyrgyz Republic'),
        ('KH', 'KHM', '116', 'Cambodia', 'Kingdom of Cambodia'),
        ('KI', 'KIR', '296', 'Kiribati', 'Republic of Kiribati'),
        ('KM', 'COM', '174', 'Comoros', 'Union of the Comoros'),
        ('KN', 'KNA', '659', 'Saint Kitts and Nevis', 'Federation of Saint Christopher and Nevis'),
        ('KP', 'PRK', '408', 'North Korea', 'Democratic People''s Republic of Korea'),
        ('KR', 'KOR', '410', 'South Korea', 'Republic of Korea'),
        ('KW', 'KWT', '414', 'Kuwait', 'State of Kuwait'),
        ('KY', 'CYM', '136', 'Cayman Islands', 'Cayman Islands'),
        ('KZ', 'KAZ', '398', 'Kazakhstan', 'Republic of Kazakhstan'),
        -- L
        ('LA', 'LAO', '418', 'Laos', 'Lao People''s Democratic Republic'),
        ('LB', 'LBN', '422', 'Lebanon', 'Lebanese Republic'),
        ('LC', 'LCA', '662', 'Saint Lucia', 'Saint Lucia'),
        ('LI', 'LIE', '438', 'Liechtenstein', 'Principality of Liechtenstein'),
        ('LK', 'LKA', '144', 'Sri Lanka', 'Democratic Socialist Republic of Sri Lanka'),
        ('LR', 'LBR', '430', 'Liberia', 'Republic of Liberia'),
        ('LS', 'LSO', '426', 'Lesotho', 'Kingdom of Lesotho'),
        ('LT', 'LTU', '440', 'Lithuania', 'Republic of Lithuania'),
        ('LU', 'LUX', '442', 'Luxembourg', 'Grand Duchy of Luxembourg'),
        ('LV', 'LVA', '428', 'Latvia', 'Republic of Latvia'),
        ('LY', 'LBY', '434', 'Libya', 'State of Libya'),
        -- M
        ('MA', 'MAR', '504', 'Morocco', 'Kingdom of Morocco'),
        ('MC', 'MCO', '492', 'Monaco', 'Principality of Monaco'),
        ('MD', 'MDA', '498', 'Moldova', 'Republic of Moldova'),
        ('ME', 'MNE', '499', 'Montenegro', 'Montenegro'),
        ('MF', 'MAF', '663', 'Saint Martin', 'Saint Martin (French part)'),
        ('MG', 'MDG', '450', 'Madagascar', 'Republic of Madagascar'),
        ('MH', 'MHL', '584', 'Marshall Islands', 'Republic of the Marshall Islands'),
        ('MK', 'MKD', '807', 'North Macedonia', 'Republic of North Macedonia'),
        ('ML', 'MLI', '466', 'Mali', 'Republic of Mali'),
        ('MM', 'MMR', '104', 'Myanmar', 'Republic of the Union of Myanmar'),
        ('MN', 'MNG', '496', 'Mongolia', 'Mongolia'),
        ('MO', 'MAC', '446', 'Macau', 'Macao Special Administrative Region of the People''s Republic of China'),
        ('MP', 'MNP', '580', 'Northern Mariana Islands', 'Commonwealth of the Northern Mariana Islands'),
        ('MQ', 'MTQ', '474', 'Martinique', 'Martinique'),
        ('MR', 'MRT', '478', 'Mauritania', 'Islamic Republic of Mauritania'),
        ('MS', 'MSR', '500', 'Montserrat', 'Montserrat'),
        ('MT', 'MLT', '470', 'Malta', 'Republic of Malta'),
        ('MU', 'MUS', '480', 'Mauritius', 'Republic of Mauritius'),
        ('MV', 'MDV', '462', 'Maldives', 'Republic of Maldives'),
        ('MW', 'MWI', '454', 'Malawi', 'Republic of Malawi'),
        ('MX', 'MEX', '484', 'Mexico', 'United Mexican States'),
        ('MY', 'MYS', '458', 'Malaysia', 'Malaysia'),
        ('MZ', 'MOZ', '508', 'Mozambique', 'Republic of Mozambique'),
        -- N
        ('NA', 'NAM', '516', 'Namibia', 'Republic of Namibia'),
        ('NC', 'NCL', '540', 'New Caledonia', 'New Caledonia'),
        ('NE', 'NER', '562', 'Niger', 'Republic of the Niger'),
        ('NF', 'NFK', '574', 'Norfolk Island', 'Norfolk Island'),
        ('NG', 'NGA', '566', 'Nigeria', 'Federal Republic of Nigeria'),
        ('NI', 'NIC', '558', 'Nicaragua', 'Republic of Nicaragua'),
        ('NL', 'NLD', '528', 'Netherlands', 'Kingdom of the Netherlands'),
        ('NO', 'NOR', '578', 'Norway', 'Kingdom of Norway'),
        ('NP', 'NPL', '524', 'Nepal', 'Federal Democratic Republic of Nepal'),
        ('NR', 'NRU', '520', 'Nauru', 'Republic of Nauru'),
        ('NU', 'NIU', '570', 'Niue', 'Niue'),
        ('NZ', 'NZL', '554', 'New Zealand', 'New Zealand'),
        -- O
        ('OM', 'OMN', '512', 'Oman', 'Sultanate of Oman'),
        -- P
        ('PA', 'PAN', '591', 'Panama', 'Republic of Panama'),
        ('PE', 'PER', '604', 'Peru', 'Republic of Peru'),
        ('PF', 'PYF', '258', 'French Polynesia', 'French Polynesia'),
        ('PG', 'PNG', '598', 'Papua New Guinea', 'Independent State of Papua New Guinea'),
        ('PH', 'PHL', '608', 'Philippines', 'Republic of the Philippines'),
        ('PK', 'PAK', '586', 'Pakistan', 'Islamic Republic of Pakistan'),
        ('PL', 'POL', '616', 'Poland', 'Republic of Poland'),
        ('PM', 'SPM', '666', 'Saint Pierre and Miquelon', 'Saint Pierre and Miquelon'),
        ('PN', 'PCN', '612', 'Pitcairn Islands', 'Pitcairn Islands'),
        ('PR', 'PRI', '630', 'Puerto Rico', 'Commonwealth of Puerto Rico'),
        ('PS', 'PSE', '275', 'Palestine', 'State of Palestine'),
        ('PT', 'PRT', '620', 'Portugal', 'Portuguese Republic'),
        ('PW', 'PLW', '585', 'Palau', 'Republic of Palau'),
        ('PY', 'PRY', '600', 'Paraguay', 'Republic of Paraguay'),
        -- Q
        ('QA', 'QAT', '634', 'Qatar', 'State of Qatar'),
        -- R
        ('RE', 'REU', '638', 'Réunion', 'Réunion'),
        ('RO', 'ROU', '642', 'Romania', 'Romania'),
        ('RS', 'SRB', '688', 'Serbia', 'Republic of Serbia'),
        ('RU', 'RUS', '643', 'Russia', 'Russian Federation'),
        ('RW', 'RWA', '646', 'Rwanda', 'Republic of Rwanda'),
        -- S
        ('SA', 'SAU', '682', 'Saudi Arabia', 'Kingdom of Saudi Arabia'),
        ('SB', 'SLB', '090', 'Solomon Islands', 'Solomon Islands'),
        ('SC', 'SYC', '690', 'Seychelles', 'Republic of Seychelles'),
        ('SD', 'SDN', '729', 'Sudan', 'Republic of the Sudan'),
        ('SE', 'SWE', '752', 'Sweden', 'Kingdom of Sweden'),
        ('SG', 'SGP', '702', 'Singapore', 'Republic of Singapore'),
        ('SH', 'SHN', '654', 'Saint Helena', 'Saint Helena, Ascension and Tristan da Cunha'),
        ('SI', 'SVN', '705', 'Slovenia', 'Republic of Slovenia'),
        ('SJ', 'SJM', '744', 'Svalbard and Jan Mayen', 'Svalbard and Jan Mayen'),
        ('SK', 'SVK', '703', 'Slovakia', 'Slovak Republic'),
        ('SL', 'SLE', '694', 'Sierra Leone', 'Republic of Sierra Leone'),
        ('SM', 'SMR', '674', 'San Marino', 'Republic of San Marino'),
        ('SN', 'SEN', '686', 'Senegal', 'Republic of Senegal'),
        ('SO', 'SOM', '706', 'Somalia', 'Federal Republic of Somalia'),
        ('SR', 'SUR', '740', 'Suriname', 'Republic of Suriname'),
        ('SS', 'SSD', '728', 'South Sudan', 'Republic of South Sudan'),
        ('ST', 'STP', '678', 'São Tomé and Príncipe', 'Democratic Republic of São Tomé and Príncipe'),
        ('SV', 'SLV', '222', 'El Salvador', 'Republic of El Salvador'),
        ('SX', 'SXM', '534', 'Sint Maarten', 'Sint Maarten (Dutch part)'),
        ('SY', 'SYR', '760', 'Syria', 'Syrian Arab Republic'),
        ('SZ', 'SWZ', '748', 'Eswatini', 'Kingdom of Eswatini'),
        -- T
        ('TC', 'TCA', '796', 'Turks and Caicos Islands', 'Turks and Caicos Islands'),
        ('TD', 'TCD', '148', 'Chad', 'Republic of Chad'),
        ('TF', 'ATF', '260', 'French Southern Territories', 'French Southern and Antarctic Lands'),
        ('TG', 'TGO', '768', 'Togo', 'Togolese Republic'),
        ('TH', 'THA', '764', 'Thailand', 'Kingdom of Thailand'),
        ('TJ', 'TJK', '762', 'Tajikistan', 'Republic of Tajikistan'),
        ('TK', 'TKL', '772', 'Tokelau', 'Tokelau'),
        ('TL', 'TLS', '626', 'Timor-Leste', 'Democratic Republic of Timor-Leste'),
        ('TM', 'TKM', '795', 'Turkmenistan', 'Turkmenistan'),
        ('TN', 'TUN', '788', 'Tunisia', 'Republic of Tunisia'),
        ('TO', 'TON', '776', 'Tonga', 'Kingdom of Tonga'),
        ('TR', 'TUR', '792', 'Turkey', 'Republic of Turkey'),
        ('TT', 'TTO', '780', 'Trinidad and Tobago', 'Republic of Trinidad and Tobago'),
        ('TV', 'TUV', '798', 'Tuvalu', 'Tuvalu'),
        ('TW', 'TWN', '158', 'Taiwan', 'Republic of China (Taiwan)'),
        ('TZ', 'TZA', '834', 'Tanzania', 'United Republic of Tanzania'),
        -- U
        ('UA', 'UKR', '804', 'Ukraine', 'Ukraine'),
        ('UG', 'UGA', '800', 'Uganda', 'Republic of Uganda'),
        ('UM', 'UMI', '581', 'U.S. Minor Outlying Islands', 'United States Minor Outlying Islands'),
        ('US', 'USA', '840', 'United States', 'United States of America'),
        ('UY', 'URY', '858', 'Uruguay', 'Oriental Republic of Uruguay'),
        ('UZ', 'UZB', '860', 'Uzbekistan', 'Republic of Uzbekistan'),
        -- V
        ('VA', 'VAT', '336', 'Vatican City', 'Vatican City State'),
        ('VC', 'VCT', '670', 'Saint Vincent and the Grenadines', 'Saint Vincent and the Grenadines'),
        ('VE', 'VEN', '862', 'Venezuela', 'Bolivarian Republic of Venezuela'),
        ('VG', 'VGB', '092', 'British Virgin Islands', 'Virgin Islands (British)'),
        ('VI', 'VIR', '850', 'U.S. Virgin Islands', 'Virgin Islands (U.S.)'),
        ('VN', 'VNM', '704', 'Vietnam', 'Socialist Republic of Vietnam'),
        ('VU', 'VUT', '548', 'Vanuatu', 'Republic of Vanuatu'),
        -- W
        ('WF', 'WLF', '876', 'Wallis and Futuna', 'Wallis and Futuna'),
        ('WS', 'WSM', '882', 'Samoa', 'Independent State of Samoa'),
        -- Y
        ('YE', 'YEM', '887', 'Yemen', 'Republic of Yemen'),
        ('YT', 'MYT', '175', 'Mayotte', 'Department of Mayotte'),
        -- Z
        ('ZA', 'ZAF', '710', 'South Africa', 'Republic of South Africa'),
        ('ZM', 'ZMB', '894', 'Zambia', 'Republic of Zambia'),
        ('ZW', 'ZWE', '716', 'Zimbabwe', 'Republic of Zimbabwe')
    ) as c(alpha2_code, alpha3_code, numeric_code, name, official_name)
    left join ores.dq_images_artefact_tbl i
        on i.dataset_id = v_flags_dataset_id
        and i.key = lower(c.alpha2_code);

    get diagnostics v_count = row_count;

    raise notice 'Successfully populated % countries for dataset: ISO 3166 Countries from Wikipedia', v_count;

    -- Report countries using placeholder flag
    raise notice 'Countries using placeholder flag (xx):';
    perform alpha2_code
    from ores.dq_countries_artefact_tbl
    where dataset_id = v_countries_dataset_id
      and image_id = v_placeholder_image_id;
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Countries Summary ---'

select 'Total DQ Countries' as metric, count(*) as count
from ores.dq_countries_artefact_tbl
union all
select 'Countries with Placeholder Flag', count(*)
from ores.dq_countries_artefact_tbl c
join ores.dq_images_artefact_tbl i on c.image_id = i.image_id
where i.key = 'xx';
