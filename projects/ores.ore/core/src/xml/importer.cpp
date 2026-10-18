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
#include "ores.ore.core/xml/importer.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/trade_mapper.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.ore.core/xml/generated/calendar_adjustments.hpp"
#include "ores.ore.core/xml/generated/currency_config.hpp"
#include "ores.ore.core/xml/projection.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/uuid/nil_generator.hpp>
#include <algorithm>
#include <format>
#include <iterator>
#include <sstream>
#include <stdexcept>

namespace ores::ore::xml {

using refdata::domain::currency;
using namespace ores::logging;

namespace {

namespace xg = generated::calendar_adjustments;

/// What an external ORE import states about itself, in the audit columns.
constexpr std::string_view audit_modified_by = "ores";
constexpr std::string_view audit_reason_code = "system.external_data_import";
constexpr std::string_view audit_commentary = "Imported from ORE XML";

/**
 * @brief Projects one calendar record into the refdata message.
 *
 * The part no table carries: which ORE element becomes which field of our own
 * message, what a date means, and the audit columns an external import
 * states.
 */
auto project(const xg::newcalendar& v) -> refdata::messaging::calendar_adjustment {
    const auto& type = projection::calendar_type();
    refdata::messaging::calendar_adjustment r;
    r.calendar_name = v.name;

    if (is_present(type, &v, "BaseCalendar"))
        r.base_calendar = v.base_calendar;

    if (is_present(type, &v, "AdditionalHolidays"))
        r.additional_holidays.assign(v.additional_holidays.date.begin(),
                                     v.additional_holidays.date.end());

    if (is_present(type, &v, "AdditionalBusinessDays"))
        r.additional_business_days.assign(v.additional_business_days.date.begin(),
                                          v.additional_business_days.date.end());

    return r;
}

namespace xc = generated::currency_config;

/**
 * @brief ORE's currency type token as our monetary nature.
 *
 * The token is carried verbatim on the entity, so this classification is
 * derived rather than stored twice.
 */
auto monetary_nature_of(std::string_view ore_type) -> std::string {
    if (ore_type.empty())
        return "";
    if (ore_type == "Metal")
        return "commodity";
    if (ore_type == "Crypto")
        return "synthetic";
    return "fiat";
}

/**
 * @brief Projects one currency record into the refdata entity.
 *
 * An element that arrived empty states no token, which is what the previous
 * mapping stored: absent and empty are one value here because ORE writes an
 * unset currency type as an empty element.
 */
auto project_currency(const xc::currency_definition& v) -> refdata::domain::currency {
    const auto& type = projection::currency_type();
    refdata::domain::currency r;
    r.iso_code = v.iso_code;
    r.name = v.name;
    r.numeric_code =
        is_present(type, &v, "NumericCode") ? std::to_string(v.numeric_code) : std::string{};
    r.symbol = v.symbol;
    r.fraction_symbol = v.fraction_symbol;
    r.fractions_per_unit = static_cast<int>(v.fractions_per_unit);
    if (const auto spelling = enumeration_spelling(type, &v, "RoundingType"))
        r.rounding_type = *spelling;
    r.rounding_precision = static_cast<int>(v.rounding_precision);
    r.format = "";
    if (is_present(type, &v, "CurrencyType") && !v.currency_type.empty())
        r.ore_currency_type = v.currency_type;
    r.monetary_nature = monetary_nature_of(r.ore_currency_type.value_or(""));
    r.market_tier = "";
    r.modified_by = std::string(audit_modified_by);
    r.change_reason_code = std::string(audit_reason_code);
    r.change_commentary = std::string(audit_commentary);
    return r;
}

}

std::string importer::validate_currency(const currency& c) {
    std::ostringstream errors;

    // Required fields per XSD
    if (c.name.empty())
        errors << "Name is required\n";

    if (c.iso_code.empty())
        errors << "ISO code is required\n";

    // Symbol and FractionSymbol are required elements in the XSD but may be
    // empty, and ORE's own example files ship them empty, so emptiness is
    // not an error.

    if (c.fractions_per_unit <= 0)
        errors << "Fractions per unit must be positive\n";

    if (c.rounding_type.empty())
        errors << "Rounding type is required\n";

    if (c.rounding_precision < 0)
        errors << "Rounding precision must be non-negative\n";

    return errors.str();
}

std::vector<currency> importer::import_currency_config(const std::filesystem::path& path) {
    BOOST_LOG_SEV(lg(), debug) << "Started import: " << path.generic_string();

    using namespace ores::platform::filesystem;
    return import_currency_config_text(file::read_content(path));
}

std::vector<currency> importer::import_currency_config_text(std::string_view text) {
    xc::currency_config record;
    const auto bound = binder::bind(xc::currency_config_document(), text, &record);
    if (!bound)
        throw std::runtime_error(
            std::format("currency config: line {}: {}", bound.error().line, bound.error().message));

    std::vector<currency> r;
    r.reserve(record.currency.size());
    std::ranges::transform(record.currency, std::back_inserter(r), project_currency);

    BOOST_LOG_SEV(lg(), debug) << "Finished importing " << r.size() << " currencies.";
    return r;
}

std::string
importer::validate_calendar_adjustment(const refdata::messaging::calendar_adjustment& ca) {
    std::ostringstream errors;

    if (ca.calendar_name.empty())
        errors << "Calendar name is required\n";

    return errors.str();
}

std::vector<refdata::messaging::calendar_adjustment>
importer::import_calendar_adjustments(const std::filesystem::path& path) {
    BOOST_LOG_SEV(lg(), debug) << "Started import: " << path.generic_string();

    using namespace ores::platform::filesystem;
    return import_calendar_adjustments_text(file::read_content(path));
}

std::vector<refdata::messaging::calendar_adjustment>
importer::import_calendar_adjustments_text(std::string_view text) {
    xg::calendaradjustment record;
    const auto bound = binder::bind(xg::calendar_adjustments_document(), text, &record);
    if (!bound)
        throw std::runtime_error(std::format(
            "calendar adjustments: line {}: {}", bound.error().line, bound.error().message));

    std::vector<refdata::messaging::calendar_adjustment> r;
    r.reserve(record.calendar.size());
    std::ranges::transform(record.calendar, std::back_inserter(r), project);

    for (auto& ca : r) {
        ca.modified_by = std::string(audit_modified_by);
        ca.change_reason_code = std::string(audit_reason_code);
        ca.change_commentary = std::string(audit_commentary);
    }

    BOOST_LOG_SEV(lg(), debug) << "Finished importing " << r.size() << " calendar adjustments.";
    return r;
}

std::vector<trade_import_item>
importer::import_portfolio_with_context(const std::filesystem::path& path) {
    BOOST_LOG_SEV(lg(), debug) << "Started portfolio import with context: "
                               << path.generic_string();

    using namespace ores::platform::filesystem;
    const std::string c(file::read_content(path));
    BOOST_LOG_SEV(lg(), trace) << "File content: " << c;

    domain::portfolio p;
    domain::load_data(c, p);

    std::vector<trade_import_item> r;
    r.reserve(p.Trade.size());

    for (const auto& t : p.Trade) {
        const auto nil = boost::uuids::nil_uuid();
        trade_import_item item;
        item.ore_id = std::string(t.id);
        item.anchor.id = nil;
        item.anchor.party_id = nil;
        item.anchor.trade_type = to_string(t.TradeType);
        item.booking.trade_id = nil;
        item.booking.book_id = nil;
        item.envelope = domain::trade_mapper::map_envelope(t);
        item.source_file = path;

        try {
            item.instrument = domain::trade_mapper::map_instrument(t);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to map instrument for trade " << std::string(t.id) << ": " << e.what();
        }

        r.push_back(std::move(item));
    }

    BOOST_LOG_SEV(lg(), debug) << "Finished importing " << r.size() << " trades with context.";
    return r;
}


ores::refdata::messaging::conventions_document
importer::import_conventions(const std::filesystem::path& path) {
    BOOST_LOG_SEV(lg(), debug) << "Started import: " << path.generic_string();

    using namespace ores::platform::filesystem;
    const std::string c(file::read_content(path));
    BOOST_LOG_SEV(lg(), trace) << "File content: " << c;

    domain::conventions conv;
    domain::load_data(c, conv);
    const auto r = domain::conventions_mapper::map(conv);

    BOOST_LOG_SEV(lg(), debug) << "Finished importing conventions from " << path.generic_string();
    return r;
}

}
