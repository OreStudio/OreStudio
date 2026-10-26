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
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/portfolio_projection.hpp"
#include "ores.ore.core/domain/pricing_engine_projection.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.ore.core/xml/emitter.hpp"
#include "ores.ore.core/xml/generated/calendar_adjustments.hpp"
#include "ores.ore.core/xml/generated/currency_config.hpp"
#include "ores.ore.core/xml/generated/pricing_engines.hpp"
#include "ores.ore.core/xml/projection.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <format>
#include <iterator>
#include <optional>
#include <pugixml.hpp>
#include <sstream>
#include <stdexcept>

namespace ores::ore::xml {

using refdata::domain::currency;
using namespace ores::logging;
using namespace ores::ore::domain;

namespace {

namespace xg = generated::calendar_adjustments;

/**
 * @brief The tables a calendar record is described by.
 *
 * A date list is two rows: the element that holds the dates, and the Date row
 * inside the record it points at. The writer writes only what its row says
 * appeared, so a projection needs both tables.
 */
using calendar_tables = std::pair<const record_type*, const record_type*>;

auto calendar_tables_of() -> const calendar_tables& {
    static const calendar_tables tables = [] {
        const auto* calendar =
            find_child_type(*xg::calendar_adjustments_document().root, "Calendar");
        if (calendar == nullptr)
            throw std::runtime_error("the calendar adjustments table has no Calendar element");
        const auto* dates = find_child_type(*calendar, "AdditionalHolidays");
        return calendar_tables{calendar, dates};
    }();
    return tables;
}

/**
 * @brief Builds one calendar record from the refdata message.
 */
auto project(const refdata::messaging::calendar_adjustment& v) -> xg::newcalendar {
    const auto& [calendar, dates] = calendar_tables_of();
    xg::newcalendar r;
    r.name = v.calendar_name;
    projection::state(*calendar, &r, "name");

    if (v.base_calendar) {
        r.base_calendar = *v.base_calendar;
        projection::state(*calendar, &r, "BaseCalendar");
    }

    const auto set_dates = [&, calendar = calendar](std::string_view name,
                                                    auto& nested,
                                                    const std::vector<std::string>& values) {
        nested.date = values;
        projection::state(*calendar, &r, name);
        if (dates != nullptr)
            projection::state(*dates, &nested, "Date");
    };

    if (!v.additional_holidays.empty())
        set_dates("AdditionalHolidays", r.additional_holidays, v.additional_holidays);

    if (!v.additional_business_days.empty())
        set_dates("AdditionalBusinessDays", r.additional_business_days, v.additional_business_days);

    return r;
}

namespace xc = generated::currency_config;

/**
 * @brief Builds one currency record from the refdata entity.
 */
auto build_currency(const refdata::domain::currency& v) -> xc::currency_definition {
    const auto& type = projection::currency_type();
    xc::currency_definition r;
    r.iso_code = v.iso_code;
    projection::state(type, &r, "ISOCode");
    r.name = v.name;
    projection::state(type, &r, "Name");
    r.symbol = v.symbol;
    projection::state(type, &r, "Symbol");
    r.fraction_symbol = v.fraction_symbol;
    projection::state(type, &r, "FractionSymbol");
    r.fractions_per_unit = v.fractions_per_unit;
    projection::state(type, &r, "FractionsPerUnit");
    r.rounding_precision = v.rounding_precision;
    projection::state(type, &r, "RoundingPrecision");

    if (!set_enumeration(type, &r, "RoundingType", v.rounding_type))
        throw std::runtime_error(std::format("'{}' is not an ORE rounding type", v.rounding_type));

    if (!v.numeric_code.empty()) {
        try {
            r.numeric_code = std::stoll(v.numeric_code);
            projection::state(type, &r, "NumericCode");
        } catch (const std::exception&) {
            // A numeric code that is not a number is left out, as before.
        }
    }

    if (v.ore_currency_type) {
        r.currency_type = *v.ore_currency_type;
        projection::state(type, &r, "CurrencyType");
    }
    return r;
}

namespace xp = generated::pricing_engines;

/**
 * @brief Writes one parameters section of a product or of the document.
 *
 * The element is written even when the list is empty, because the document
 * states every section rather than leaving out the ones it has nothing to put
 * in. The order is the order the rows carry, and nothing is sorted here: the
 * projection has already put them back in it.
 */
auto write_parameters(pugi::xml_node parent,
                      const char* name,
                      const std::vector<xp::parameter>& parameters) -> void {
    auto section = parent.append_child(name);
    for (const auto& parameter : parameters) {
        auto node = section.append_child("Parameter");
        node.append_attribute("name").set_value(parameter.name.c_str());
        node.text().set(parameter.value.c_str());
    }
}

} // namespace

std::string exporter::export_currency_config(const std::vector<currency>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Exporting currency config. Total: " << v.size();

    xc::currency_config record;
    record.currency.reserve(v.size());
    std::ranges::transform(v, std::back_inserter(record.currency), build_currency);
    projection::state(*xc::currency_config_document().root, &record, "Currency");

    std::string out;
    const auto emitted = emitter::emit(xc::currency_config_document(), &record, out);
    if (!emitted)
        throw std::runtime_error(emitted.error().message);
    return out;
}

std::string
exporter::export_pricing_engines(const analytics::messaging::pricing_engines_document& v) {
    BOOST_LOG_SEV(lg(), debug) << "Exporting pricing engines. Products: " << v.products.size();

    // The projection is the one statement of which parameter belongs to which
    // product, of what a row the document has no place for is refused with, and
    // of the order the rows were written in, so the writer adds no rule of its
    // own on top of it.
    const auto record = domain::pricing_engine_projection::reverse(v);

    pugi::xml_document document;
    auto declaration = document.append_child(pugi::node_declaration);
    declaration.append_attribute("version").set_value("1.0");

    auto root = document.append_child("PricingEngines");
    for (const auto& product : record.product) {
        auto node = root.append_child("Product");
        node.append_attribute("type").set_value(product.type.c_str());
        node.append_child("Model").text().set(product.model.c_str());
        write_parameters(node, "ModelParameters", product.model_parameters.parameter);
        node.append_child("Engine").text().set(product.engine.c_str());
        write_parameters(node, "EngineParameters", product.engine_parameters.parameter);
    }
    // One block, after the products, holding the parameters no product owns.
    write_parameters(root, "GlobalParameters", record.global_parameters.parameter);

    std::ostringstream out;
    document.save(out, "  ", pugi::format_default, pugi::encoding_utf8);
    return out.str();
}

std::string exporter::export_calendar_adjustments(
    const std::vector<refdata::messaging::calendar_adjustment>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Exporting " << v.size() << " calendar adjustments.";
    const auto* calendar = find_child_type(*xg::calendar_adjustments_document().root, "Calendar");
    if (calendar == nullptr)
        throw std::runtime_error("the calendar adjustments table has no Calendar element");

    xg::calendaradjustment record;
    record.calendar.reserve(v.size());
    std::ranges::transform(v, std::back_inserter(record.calendar), project);
    // The Calendar element is repeated, and a document with none states an
    // empty list rather than nothing.
    projection::state(*xg::calendar_adjustments_document().root, &record, "Calendar");

    std::string out;
    const auto emitted = emitter::emit(
        generated::calendar_adjustments::calendar_adjustments_document(), &record, out);
    if (!emitted)
        throw std::runtime_error(emitted.error().message);
    return out;
}

std::string exporter::export_conventions(const domain::conventions_package& package) {
    BOOST_LOG_SEV(lg(), debug) << "Exporting conventions.";
    return domain::conventions_mapper::write(domain::join_conventions(package));
}

std::string
exporter::export_portfolio(const std::vector<trading::messaging::trade_export_item>& items,
                           const trading::domain::instrument_batch& instruments,
                           const reference_curve_names& reference_curves) {
    BOOST_LOG_SEV(lg(), debug) << "Started portfolio export. Items: " << items.size();

    const auto document = portfolio_projection::reverse(items, instruments, reference_curves);
    auto result = portfolio_projection::write(document);

    BOOST_LOG_SEV(lg(), debug) << "Finished portfolio export. Trades: " << document.trade.size();
    return result;
}

}
