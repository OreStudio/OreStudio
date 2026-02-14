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
#include "ores.refdata/generators/currency_generator.hpp"

#include <atomic>
#include <unordered_set>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <faker-cxx/finance.h>
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;

domain::currency generate_synthetic_currency(
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");
    const auto tid = ctx.env().get_or(
        std::string(generation_keys::tenant_id), "system");

    domain::currency r;

    const auto parsed_tid = utility::uuid::tenant_id::from_string(tid);
    r.tenant_id = parsed_tid.has_value() ? parsed_tid.value()
        : utility::uuid::tenant_id::system();

    // Use fictional currency with unique code to avoid conflicts across tests
    static std::atomic<std::size_t> counter{0};
    const auto suffix = ++counter;
    r.iso_code = "X" + std::to_string(suffix % 100).substr(0, 2) +
        std::to_string(suffix);
    r.name = "Test Currency " + std::to_string(suffix);
    r.numeric_code = std::to_string(10000 + suffix);
    r.symbol = "T" + std::to_string(suffix % 10);
    r.fraction_symbol = "";
    r.fractions_per_unit = 100;
    r.rounding_type = "Closest";
    r.rounding_precision = 2;
    r.format = "%3% %1$.2f";
    r.currency_type = "Fiat";
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.modified_by = modified_by;
    r.recorded_at = ctx.past_timepoint();

    return r;
}

std::vector<domain::currency> generate_synthetic_unicode_currencies(
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");
    const auto tid = ctx.env().get_or(
        std::string(generation_keys::tenant_id), "system");
    const auto parsed_tid = utility::uuid::tenant_id::from_string(tid);
    const auto tenant_id = parsed_tid.has_value() ? parsed_tid.value()
        : utility::uuid::tenant_id::system();

    // Use fictional ISO codes with unique suffix to avoid conflicts across tests
    static std::atomic<std::size_t> batch{0};
    const auto b = ++batch;
    const auto suffix = "_" + std::to_string(b);
    const auto now = ctx.past_timepoint();

    std::vector<domain::currency> r;
    r.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XU" + std::to_string(b),  // Fictional USD-like
        .name = "Test Dollar" + suffix,
        .numeric_code = "90001",
        .symbol = "$",
        .fraction_symbol = "\xC2\xA2",
        .fractions_per_unit = 100,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });

    r.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XE" + std::to_string(b),  // Fictional EUR-like
        .name = "Test Euro" + suffix,
        .numeric_code = "90002",
        .symbol = "\xE2\x82\xAC",
        .fraction_symbol = "\xC2\xA2",
        .fractions_per_unit = 100,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });

    r.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XG" + std::to_string(b),  // Fictional GBP-like
        .name = "Test Pound" + suffix,
        .numeric_code = "90003",
        .symbol = "\xC2\xA3",
        .fraction_symbol = "p",
        .fractions_per_unit = 100,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });

    r.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XJ" + std::to_string(b),  // Fictional JPY-like (no fractions)
        .name = "Test Yen" + suffix,
        .numeric_code = "90004",
        .symbol = "\xC2\xA5",
        .fractions_per_unit = 0,
        .rounding_type = "Closest",
        .rounding_precision = 0,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });

    r.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XI" + std::to_string(b),  // Fictional INR-like
        .name = "Test Rupee" + suffix,
        .numeric_code = "90005",
        .symbol = "\xE2\x82\xB9",
        .fraction_symbol = "\xE0\xA4\xAA",
        .fractions_per_unit = 100,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });

    r.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XB" + std::to_string(b),  // Fictional BTC-like
        .name = "Test Crypto" + suffix,
        .numeric_code = "90006",
        .symbol = "\xE2\x82\xBF",
        .fraction_symbol = "s",
        .fractions_per_unit = 100000000,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });

    r.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XR" + std::to_string(b),  // Fictional RUB-like
        .name = "Test Ruble" + suffix,
        .numeric_code = "90007",
        .symbol = "\xE2\x82\xBD",
        .fraction_symbol = "\xD0\xBA",
        .fractions_per_unit = 100,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .modified_by = modified_by,
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = now
    });
    return r;
}

std::vector<domain::currency>
generate_synthetic_currencies(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<domain::currency> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_currency(ctx));

    return r;
}

std::vector<domain::currency>
generate_unique_synthetic_currencies(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::unordered_set<std::string> seen;
    seen.reserve(n);

    std::vector<domain::currency> r;
    r.reserve(n);

    std::size_t suffix = 0;
    while (r.size() < n) {
        auto currency = generate_synthetic_currency(ctx);
        // Loop until we find a unique key
        if (!seen.insert(currency.iso_code).second) {
            auto base_code = currency.iso_code;
            do {
                currency.iso_code = base_code + std::to_string(++suffix);
            } while (!seen.insert(currency.iso_code).second);
        }
        r.push_back(std::move(currency));
    }
    return r;
}

std::vector<domain::currency>
generate_fictional_currencies(std::size_t n,
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        std::string(generation_keys::modified_by), "system");
    const auto tid = ctx.env().get_or(
        std::string(generation_keys::tenant_id), "system");
    const auto parsed_tid = utility::uuid::tenant_id::from_string(tid);
    const auto tenant_id = parsed_tid.has_value() ? parsed_tid.value()
        : utility::uuid::tenant_id::system();
    const auto now = ctx.past_timepoint();

    std::vector<domain::currency> all;
    all.reserve(50);

    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XAE", .name = "Aerilonian Dollar", .numeric_code = "10001",
        .symbol = "A$", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XAR", .name = "Arcturian Arct", .numeric_code = "10002",
        .symbol = "Ar", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XBL", .name = "Balthorian Florin", .numeric_code = "10003",
        .symbol = "Bf", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XBE", .name = "Bellorian Bell", .numeric_code = "10004",
        .symbol = "Bb", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XCA", .name = "Calandrian Crown", .numeric_code = "10005",
        .symbol = "Cc", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XCL", .name = "Caledonian Caled", .numeric_code = "10006",
        .symbol = "Cd", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XDA", .name = "Daelorian Dinar", .numeric_code = "10007",
        .symbol = "Dd", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XDL", .name = "Delvadian Delv", .numeric_code = "10008",
        .symbol = "De", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XER", .name = "Eriadoran Euro", .numeric_code = "10009",
        .symbol = "Er", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XES", .name = "Esterian Est", .numeric_code = "10010",
        .symbol = "Es", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XFE", .name = "Felorian Franc", .numeric_code = "10011",
        .symbol = "Ff", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XFN", .name = "Fendarian Fen", .numeric_code = "10012",
        .symbol = "Fn", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XGA", .name = "Galdorian Galleon", .numeric_code = "10013",
        .symbol = "Gg", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XGR", .name = "Grendorian Grend", .numeric_code = "10014",
        .symbol = "Gr", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XHE", .name = "Helvetian Franc", .numeric_code = "10015",
        .symbol = "Hf", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XHY", .name = "Hydronian Hyd", .numeric_code = "10016",
        .symbol = "Hy", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XIR", .name = "Iridian Dollar", .numeric_code = "10017",
        .symbol = "I$", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XIT", .name = "Ithacan Ith", .numeric_code = "10018",
        .symbol = "It", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XJE", .name = "Jethronian Jet", .numeric_code = "10019",
        .symbol = "Je", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XJO", .name = "Jorvikian Krona", .numeric_code = "10020",
        .symbol = "Jk", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XKA", .name = "Kaelorian Krown", .numeric_code = "10021",
        .symbol = "Kk", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XKR", .name = "Krynnish Krynn", .numeric_code = "10022",
        .symbol = "Kr", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XLU", .name = "Luminian Lum", .numeric_code = "10023",
        .symbol = "Lu", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XLY", .name = "Lysandrian Lira", .numeric_code = "10024",
        .symbol = "Ly", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XMA", .name = "Maldorian Mal", .numeric_code = "10025",
        .symbol = "Mm", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XMR", .name = "Mariposan Peso", .numeric_code = "10026",
        .symbol = "Mp", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XNE", .name = "Nektonian Nek", .numeric_code = "10027",
        .symbol = "Ne", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XNT", .name = "Netharian Naira", .numeric_code = "10028",
        .symbol = "Nt", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XOR", .name = "Orinocan Bolivar", .numeric_code = "10029",
        .symbol = "Ob", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XOL", .name = "Orlanthian Orl", .numeric_code = "10030",
        .symbol = "Ol", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XPL", .name = "Paldorian Peso", .numeric_code = "10031",
        .symbol = "Pp", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XPY", .name = "Pyrrhian Pyr", .numeric_code = "10032",
        .symbol = "Py", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XQU", .name = "Quentarian Quen", .numeric_code = "10033",
        .symbol = "Qu", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XQN", .name = "Quinarian Quetzal", .numeric_code = "10034",
        .symbol = "Qn", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XRE", .name = "Rendellian Rend", .numeric_code = "10035",
        .symbol = "Re", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XRI", .name = "Rivenian Ruble", .numeric_code = "10036",
        .symbol = "Ri", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XSE", .name = "Serendian Shilling", .numeric_code = "10037",
        .symbol = "Ss", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XSI", .name = "Sildorian Sild", .numeric_code = "10038",
        .symbol = "Si", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XTA", .name = "Tandorian Taka", .numeric_code = "10039",
        .symbol = "Ta", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XTE", .name = "Tenebrian Ten", .numeric_code = "10040",
        .symbol = "Te", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XUL", .name = "Uldorian Uld", .numeric_code = "10041",
        .symbol = "Ul", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XUT", .name = "Utopian Unit", .numeric_code = "10042",
        .symbol = "Ut", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XVA", .name = "Valorian Valt", .numeric_code = "10043",
        .symbol = "Vv", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XVL", .name = "Valtarian Val", .numeric_code = "10044",
        .symbol = "Vl", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XWI", .name = "Wintervalean Won", .numeric_code = "10045",
        .symbol = "Ww", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XWY", .name = "Wysterian Wys", .numeric_code = "10046",
        .symbol = "Wy", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XAN", .name = "Xandrian Xan", .numeric_code = "10047",
        .symbol = "Xa", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XEN", .name = "Xenorian Xen", .numeric_code = "10048",
        .symbol = "Xe", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XYS", .name = "Yslandian Yen", .numeric_code = "10049",
        .symbol = "Ys", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .tenant_id = tenant_id,
        .iso_code = "XZE", .name = "Zephyrian Zephyr", .numeric_code = "10050",
        .symbol = "Ze", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .modified_by = modified_by, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });

    if (n == 0 || n >= all.size())
        return all;

    return std::vector<domain::currency>(all.begin(), all.begin() + n);
}

}
