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

#include <unordered_set>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <faker-cxx/finance.h>
#include "ores.utility/faker/datetime.hpp"

namespace ores::refdata::generators {

domain::currency generate_synthetic_currency() {
    domain::currency r;

    auto fakerCurrency = faker::finance::currency();
    r.iso_code = fakerCurrency.code;
    r.name = std::string(fakerCurrency.name);
    r.numeric_code = std::to_string(faker::number::integer(1, 999));
    // Some faker currencies don't have symbols; provide fallback using code
    r.symbol = std::string(fakerCurrency.symbol.empty()
        ? fakerCurrency.code
        : fakerCurrency.symbol);
    r.fraction_symbol = "";
    r.fractions_per_unit = 100;
    r.rounding_type = "Closest";
    r.rounding_precision = 2;
    r.format = "%3% %1$.2f";
    r.currency_type = "Fiat";
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_by = std::string(faker::internet::username());
    r.recorded_at = utility::faker::datetime::past_timepoint();

    return r;
}

std::vector<domain::currency> generate_synthetic_unicode_currencies() {
    std::vector<domain::currency> r;
    r.push_back({
        .iso_code = "USD",
        .name = "United States Dollar",
        .numeric_code = "840",
        .symbol = "$",
        .fraction_symbol = "¢",
        .fractions_per_unit = 100,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .recorded_by = std::string(faker::internet::username()),
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = utility::faker::datetime::past_timepoint()
    });

    r.push_back({
        .iso_code = "EUR",
        .name = "Euro",
        .numeric_code = "110",
        .symbol = "€",
        .fraction_symbol = "¢",
        .fractions_per_unit = 100,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .recorded_by = std::string(faker::internet::username()),
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = utility::faker::datetime::past_timepoint()
    });

    r.push_back({
        .iso_code = "GBP",
        .name = "British Pound Sterling",
        .numeric_code = "110",
        .symbol = "£",
        .fraction_symbol = "p",
        .fractions_per_unit = 100,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .recorded_by = std::string(faker::internet::username()),
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = utility::faker::datetime::past_timepoint()
    });

    r.push_back({
        .iso_code = "JPY",
        .name = "Japanese Yen",
        .numeric_code = "110",
        .symbol = "¥",
        .fractions_per_unit = 0,
        .rounding_type = "Closest",
        .rounding_precision = 0,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .recorded_by = std::string(faker::internet::username()),
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = utility::faker::datetime::past_timepoint()
    });

    r.push_back({
        .iso_code = "INR",
        .name = "Indian Rupee",
        .numeric_code = "110",
        .symbol = "₹",
        .fraction_symbol = "प",
        .fractions_per_unit = 100,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .recorded_by = std::string(faker::internet::username()),
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = utility::faker::datetime::past_timepoint()
    });

    r.push_back({
        .iso_code = "BTC",
        .name = "Bitcoin",
        .numeric_code = "110",
        .symbol = "₿",
        .fraction_symbol = "s",
        .fractions_per_unit = 100000000,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .recorded_by = std::string(faker::internet::username()),
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = utility::faker::datetime::past_timepoint()
    });

    r.push_back({
        .iso_code = "RUB",
        .name = "Russian Rubble",
        .numeric_code = "110",
        .symbol = "₽",
        .fraction_symbol = "к",
        .fractions_per_unit = 100,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .recorded_by = std::string(faker::internet::username()),
        .change_reason_code = "system.test",
        .change_commentary = "Synthetic test data",
        .recorded_at = utility::faker::datetime::past_timepoint()
    });
    return r;
}

std::vector<domain::currency>
generate_synthetic_currencies(std::size_t n) {
    std::vector<domain::currency> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_currency());

    return r;
}

std::vector<domain::currency>
generate_unique_synthetic_currencies(std::size_t n) {
    std::unordered_set<std::string> seen;
    seen.reserve(n);

    std::vector<domain::currency> r;
    r.reserve(n);

    std::size_t suffix = 0;
    while (r.size() < n) {
        auto currency = generate_synthetic_currency();
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
generate_fictional_currencies(std::size_t n) {
    const auto now = std::chrono::system_clock::now();
    const auto user = std::string(faker::internet::username());

    std::vector<domain::currency> all;
    all.reserve(50);

    all.push_back({
        .iso_code = "XAE", .name = "Aerilonian Dollar", .numeric_code = "10001",
        .symbol = "A$", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XAR", .name = "Arcturian Arct", .numeric_code = "10002",
        .symbol = "Ar", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XBL", .name = "Balthorian Florin", .numeric_code = "10003",
        .symbol = "Bf", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XBE", .name = "Bellorian Bell", .numeric_code = "10004",
        .symbol = "Bb", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XCA", .name = "Calandrian Crown", .numeric_code = "10005",
        .symbol = "Cc", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XCL", .name = "Caledonian Caled", .numeric_code = "10006",
        .symbol = "Cd", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XDA", .name = "Daelorian Dinar", .numeric_code = "10007",
        .symbol = "Dd", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XDL", .name = "Delvadian Delv", .numeric_code = "10008",
        .symbol = "De", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XER", .name = "Eriadoran Euro", .numeric_code = "10009",
        .symbol = "Er", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XES", .name = "Esterian Est", .numeric_code = "10010",
        .symbol = "Es", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XFE", .name = "Felorian Franc", .numeric_code = "10011",
        .symbol = "Ff", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XFN", .name = "Fendarian Fen", .numeric_code = "10012",
        .symbol = "Fn", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XGA", .name = "Galdorian Galleon", .numeric_code = "10013",
        .symbol = "Gg", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XGR", .name = "Grendorian Grend", .numeric_code = "10014",
        .symbol = "Gr", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XHE", .name = "Helvetian Franc", .numeric_code = "10015",
        .symbol = "Hf", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XHY", .name = "Hydronian Hyd", .numeric_code = "10016",
        .symbol = "Hy", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XIR", .name = "Iridian Dollar", .numeric_code = "10017",
        .symbol = "I$", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XIT", .name = "Ithacan Ith", .numeric_code = "10018",
        .symbol = "It", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XJE", .name = "Jethronian Jet", .numeric_code = "10019",
        .symbol = "Je", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XJO", .name = "Jorvikian Krona", .numeric_code = "10020",
        .symbol = "Jk", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XKA", .name = "Kaelorian Krown", .numeric_code = "10021",
        .symbol = "Kk", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XKR", .name = "Krynnish Krynn", .numeric_code = "10022",
        .symbol = "Kr", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XLU", .name = "Luminian Lum", .numeric_code = "10023",
        .symbol = "Lu", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XLY", .name = "Lysandrian Lira", .numeric_code = "10024",
        .symbol = "Ly", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XMA", .name = "Maldorian Mal", .numeric_code = "10025",
        .symbol = "Mm", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XMR", .name = "Mariposan Peso", .numeric_code = "10026",
        .symbol = "Mp", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XNE", .name = "Nektonian Nek", .numeric_code = "10027",
        .symbol = "Ne", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XNT", .name = "Netharian Naira", .numeric_code = "10028",
        .symbol = "Nt", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XOR", .name = "Orinocan Bolivar", .numeric_code = "10029",
        .symbol = "Ob", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XOL", .name = "Orlanthian Orl", .numeric_code = "10030",
        .symbol = "Ol", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XPL", .name = "Paldorian Peso", .numeric_code = "10031",
        .symbol = "Pp", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XPY", .name = "Pyrrhian Pyr", .numeric_code = "10032",
        .symbol = "Py", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XQU", .name = "Quentarian Quen", .numeric_code = "10033",
        .symbol = "Qu", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XQN", .name = "Quinarian Quetzal", .numeric_code = "10034",
        .symbol = "Qn", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XRE", .name = "Rendellian Rend", .numeric_code = "10035",
        .symbol = "Re", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XRI", .name = "Rivenian Ruble", .numeric_code = "10036",
        .symbol = "Ri", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XSE", .name = "Serendian Shilling", .numeric_code = "10037",
        .symbol = "Ss", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XSI", .name = "Sildorian Sild", .numeric_code = "10038",
        .symbol = "Si", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XTA", .name = "Tandorian Taka", .numeric_code = "10039",
        .symbol = "Ta", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XTE", .name = "Tenebrian Ten", .numeric_code = "10040",
        .symbol = "Te", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XUL", .name = "Uldorian Uld", .numeric_code = "10041",
        .symbol = "Ul", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XUT", .name = "Utopian Unit", .numeric_code = "10042",
        .symbol = "Ut", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XVA", .name = "Valorian Valt", .numeric_code = "10043",
        .symbol = "Vv", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XVL", .name = "Valtarian Val", .numeric_code = "10044",
        .symbol = "Vl", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XWI", .name = "Wintervalean Won", .numeric_code = "10045",
        .symbol = "Ww", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XWY", .name = "Wysterian Wys", .numeric_code = "10046",
        .symbol = "Wy", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XAN", .name = "Xandrian Xan", .numeric_code = "10047",
        .symbol = "Xa", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XEN", .name = "Xenorian Xen", .numeric_code = "10048",
        .symbol = "Xe", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XYS", .name = "Yslandian Yen", .numeric_code = "10049",
        .symbol = "Ys", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .iso_code = "XZE", .name = "Zephyrian Zephyr", .numeric_code = "10050",
        .symbol = "Ze", .fractions_per_unit = 100, .rounding_type = "Closest",
        .rounding_precision = 2, .format = "%3% %1$.2f", .currency_type = "Fiat",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });

    if (n == 0 || n >= all.size())
        return all;

    return std::vector<domain::currency>(all.begin(), all.begin() + n);
}

}
