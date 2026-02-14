/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
#include "ores.refdata/generators/country_generator.hpp"

#include "ores.utility/generation/generation_keys.hpp"

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;

std::vector<domain::country> generate_fictional_countries(std::size_t n,
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        generation_keys::modified_by, "system");
    const auto now = ctx.past_timepoint();

    std::vector<domain::country> all;
    all.reserve(50);

    all.push_back({
        .alpha2_code = "AL", .alpha3_code = "AER", .numeric_code = "10001",
        .name = "Aerilon", .official_name = "Republic of Aerilon",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "AR", .alpha3_code = "ARC", .numeric_code = "10002",
        .name = "Arcturia", .official_name = "Federation of Arcturia",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "BA", .alpha3_code = "BAL", .numeric_code = "10003",
        .name = "Balthoria", .official_name = "Kingdom of Balthoria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "BE", .alpha3_code = "BEL", .numeric_code = "10004",
        .name = "Belloria", .official_name = "Principality of Belloria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "CA", .alpha3_code = "CAL", .numeric_code = "10005",
        .name = "Calandria", .official_name = "Empire of Calandria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "CD", .alpha3_code = "CLD", .numeric_code = "10006",
        .name = "Caledonia", .official_name = "Commonwealth of Caledonia",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "DA", .alpha3_code = "DAE", .numeric_code = "10007",
        .name = "Daeloria", .official_name = "Republic of Daeloria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "DE", .alpha3_code = "DEL", .numeric_code = "10008",
        .name = "Delvadia", .official_name = "Duchy of Delvadia",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "ER", .alpha3_code = "ERI", .numeric_code = "10009",
        .name = "Eriador", .official_name = "United Realms of Eriador",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "ES", .alpha3_code = "EST", .numeric_code = "10010",
        .name = "Esteria", .official_name = "Republic of Esteria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "FE", .alpha3_code = "FEL", .numeric_code = "10011",
        .name = "Feloria", .official_name = "Federation of Feloria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "FN", .alpha3_code = "FEN", .numeric_code = "10012",
        .name = "Fendaria", .official_name = "Republic of Fendaria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "GA", .alpha3_code = "GAL", .numeric_code = "10013",
        .name = "Galdoria", .official_name = "Kingdom of Galdoria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "GR", .alpha3_code = "GRN", .numeric_code = "10014",
        .name = "Grendoria", .official_name = "Empire of Grendoria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "HE", .alpha3_code = "HEL", .numeric_code = "10015",
        .name = "Helvetia", .official_name = "Confederation of Helvetia",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "HY", .alpha3_code = "HYD", .numeric_code = "10016",
        .name = "Hydronia", .official_name = "Republic of Hydronia",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "IR", .alpha3_code = "IRI", .numeric_code = "10017",
        .name = "Iridia", .official_name = "Commonwealth of Iridia",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "IT", .alpha3_code = "ITH", .numeric_code = "10018",
        .name = "Ithaca", .official_name = "Republic of Ithaca",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "JE", .alpha3_code = "JET", .numeric_code = "10019",
        .name = "Jethro", .official_name = "Kingdom of Jethro",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "JO", .alpha3_code = "JOR", .numeric_code = "10020",
        .name = "Jorvik", .official_name = "Kingdom of Jorvik",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "KA", .alpha3_code = "KAE", .numeric_code = "10021",
        .name = "Kaelor", .official_name = "Empire of Kaelor",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "KR", .alpha3_code = "KRY", .numeric_code = "10022",
        .name = "Krynn", .official_name = "Federation of Krynn",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "LU", .alpha3_code = "LUM", .numeric_code = "10023",
        .name = "Luminia", .official_name = "Republic of Luminia",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "LY", .alpha3_code = "LYS", .numeric_code = "10024",
        .name = "Lysandria", .official_name = "Principality of Lysandria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "MA", .alpha3_code = "MAL", .numeric_code = "10025",
        .name = "Maldoria", .official_name = "Kingdom of Maldoria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "MR", .alpha3_code = "MRP", .numeric_code = "10026",
        .name = "Mariposa", .official_name = "Republic of Mariposa",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "NE", .alpha3_code = "NEK", .numeric_code = "10027",
        .name = "Nektonia", .official_name = "Federation of Nektonia",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "NT", .alpha3_code = "NTH", .numeric_code = "10028",
        .name = "Netharia", .official_name = "Commonwealth of Netharia",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "OR", .alpha3_code = "ORI", .numeric_code = "10029",
        .name = "Orinoco", .official_name = "Republic of Orinoco",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "OL", .alpha3_code = "ORL", .numeric_code = "10030",
        .name = "Orlanthia", .official_name = "Empire of Orlanthia",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "PA", .alpha3_code = "PAL", .numeric_code = "10031",
        .name = "Paldoria", .official_name = "Kingdom of Paldoria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "PY", .alpha3_code = "PYR", .numeric_code = "10032",
        .name = "Pyrrhia", .official_name = "Republic of Pyrrhia",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "QU", .alpha3_code = "QUE", .numeric_code = "10033",
        .name = "Quentaria", .official_name = "Duchy of Quentaria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "QN", .alpha3_code = "QUI", .numeric_code = "10034",
        .name = "Quinaria", .official_name = "Federation of Quinaria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "RE", .alpha3_code = "REN", .numeric_code = "10035",
        .name = "Rendellia", .official_name = "Republic of Rendellia",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "RI", .alpha3_code = "RIV", .numeric_code = "10036",
        .name = "Rivenia", .official_name = "Commonwealth of Rivenia",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "SE", .alpha3_code = "SER", .numeric_code = "10037",
        .name = "Serendia", .official_name = "Kingdom of Serendia",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "SI", .alpha3_code = "SIL", .numeric_code = "10038",
        .name = "Sildoria", .official_name = "Empire of Sildoria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "TA", .alpha3_code = "TAN", .numeric_code = "10039",
        .name = "Tandor", .official_name = "Republic of Tandor",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "TE", .alpha3_code = "TEN", .numeric_code = "10040",
        .name = "Tenebria", .official_name = "Federation of Tenebria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "UL", .alpha3_code = "ULD", .numeric_code = "10041",
        .name = "Uldoria", .official_name = "Kingdom of Uldoria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "UT", .alpha3_code = "UTP", .numeric_code = "10042",
        .name = "Utopia", .official_name = "Republic of Utopia",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "VA", .alpha3_code = "VAL", .numeric_code = "10043",
        .name = "Valoria", .official_name = "Empire of Valoria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "VL", .alpha3_code = "VLT", .numeric_code = "10044",
        .name = "Valtaria", .official_name = "Principality of Valtaria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "WI", .alpha3_code = "WIN", .numeric_code = "10045",
        .name = "Wintervale", .official_name = "Commonwealth of Wintervale",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "WY", .alpha3_code = "WYS", .numeric_code = "10046",
        .name = "Wysteria", .official_name = "Republic of Wysteria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "XA", .alpha3_code = "XAN", .numeric_code = "10047",
        .name = "Xandria", .official_name = "Kingdom of Xandria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "XE", .alpha3_code = "XEN", .numeric_code = "10048",
        .name = "Xenoria", .official_name = "Federation of Xenoria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "YS", .alpha3_code = "YSL", .numeric_code = "10049",
        .name = "Yslandia", .official_name = "Republic of Yslandia",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .alpha2_code = "ZE", .alpha3_code = "ZEP", .numeric_code = "10050",
        .name = "Zephyria", .official_name = "Empire of Zephyria",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });

    if (n == 0 || n >= all.size())
        return all;

    return std::vector<domain::country>(all.begin(), all.begin() + n);
}

}
