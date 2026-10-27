/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
#include "ores.ore.core/domain/portfolio_instrument_projection.hpp"
#include <cctype>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

namespace ores::ore::domain {

namespace {

using trading::domain::scripted_instrument;

/**
 * @brief A scripted instrument with the audit columns an import states.
 *
 * The trading domain keeps the audit beside the product, and every family's
 * reading sets the same four columns.
 */
auto scripted_instrument_projection_make_base(std::string_view trade_type_code)
    -> scripted_instrument {
    scripted_instrument r;
    r.identity.trade_type_code = std::string(trade_type_code);
    r.audit.modified_by = "ores";
    r.audit.performed_by = "ores";
    r.audit.change_reason_code = "system.external_data_import";
    r.audit.change_commentary = "Imported from ORE XML";
    return r;
}

auto scripted_instrument_projection_json_escape(std::string_view text) -> std::string {
    std::string out;
    for (const char c : text) {
        if (c == '"')
            out += "\\\"";
        else if (c == '\\')
            out += "\\\\";
        else
            out += c;
    }
    return out;
}

auto json_array(const std::vector<std::string>& values) -> std::string {
    std::string out = "[";
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (i != 0)
            out.push_back(',');
        out += "\"" + scripted_instrument_projection_json_escape(values[i]) + "\"";
    }
    out += "]";
    return out;
}

auto number_text(std::string_view value) -> double {
    try {
        return static_cast<double>(std::stof(std::string(value)));
    } catch (const std::exception&) {
        return 0;
    }
}

auto make_params_json(std::string_view put_call, double notional, double strike) -> std::string {
    return "{\"PutCall\":\"" + std::string(put_call) +
           "\",\"Notional\":" + std::to_string(notional) + ",\"Strike\":" + std::to_string(strike) +
           "}";
}

auto parse_json_string_array(std::string_view json) -> std::vector<std::string> {
    std::vector<std::string> result;
    std::size_t i = 0;
    while (i < json.size() && json[i] != '[')
        ++i;
    if (i >= json.size())
        return result;
    ++i;
    while (i < json.size()) {
        while (i < json.size() && (json[i] == ' ' || json[i] == ',' || json[i] == '\t'))
            ++i;
        if (i >= json.size() || json[i] == ']')
            break;
        if (json[i] != '"') {
            ++i;
            continue;
        }
        ++i;
        std::string value;
        while (i < json.size() && json[i] != '"') {
            if (json[i] == '\\' && i + 1 < json.size()) {
                ++i;
                if (json[i] == '"')
                    value += '"';
                else if (json[i] == '\\')
                    value += '\\';
                else
                    value += json[i];
            } else {
                value += json[i];
            }
            ++i;
        }
        ++i;
        result.push_back(std::move(value));
    }
    return result;
}

auto parse_json_object(std::string_view json) -> std::vector<std::pair<std::string, std::string>> {
    std::vector<std::pair<std::string, std::string>> result;
    std::size_t i = 0;
    while (i < json.size() && json[i] != '{')
        ++i;
    if (i >= json.size())
        return result;
    ++i;
    while (i < json.size()) {
        while (i < json.size() && (json[i] == ' ' || json[i] == ',' || json[i] == '\t'))
            ++i;
        if (i >= json.size() || json[i] == '}')
            break;
        if (json[i] != '"') {
            ++i;
            continue;
        }
        ++i;
        std::string key;
        while (i < json.size() && json[i] != '"')
            key += json[i++];
        ++i;
        while (i < json.size() && json[i] != ':')
            ++i;
        ++i;
        while (i < json.size() && (json[i] == ' ' || json[i] == '\t'))
            ++i;
        std::string value;
        if (i < json.size() && json[i] == '"') {
            ++i;
            while (i < json.size() && json[i] != '"') {
                if (json[i] == '\\' && i + 1 < json.size()) {
                    ++i;
                    value += json[i];
                } else {
                    value += json[i];
                }
                ++i;
            }
            ++i;
        } else {
            while (i < json.size() && json[i] != ',' && json[i] != '}')
                value += json[i++];
        }
        result.emplace_back(std::move(key), std::move(value));
    }
    return result;
}

auto own_text(const xml::record_reader& record, std::string_view name) -> std::string {
    const auto child = record.child(name);
    if (!child)
        return {};
    return child->own_text().value_or("");
}

auto vector_values(const xml::record_reader& record, std::string_view name)
    -> std::vector<std::string> {
    const auto child = record.child(name);
    if (!child)
        return {};
    return child->values("Value");
}

/**
 * @brief The parameter key that carries one underlying's type.
 *
 * The schema requires an underlying to state a type and the flat table has no
 * column for it, so the type travels in the parameters beside the names the
 * underlying list carries.
 */
auto underlying_type_key(std::size_t index) -> std::string {
    return "Underlying" + std::to_string(index + 1) + "Type";
}

/// The underlying a parameter key names the type of, when the key is one.
auto underlying_of_type_key(std::string_view key) -> std::optional<std::string> {
    constexpr std::string_view prefix = "Underlying";
    constexpr std::string_view suffix = "Type";
    if (!key.starts_with(prefix) || !key.ends_with(suffix) ||
        key.size() <= prefix.size() + suffix.size())
        return std::nullopt;
    return std::string(key.substr(0, key.size() - suffix.size()));
}

/// The type entries a parameter list states, in the order it states them.
auto underlying_types_of(const std::vector<std::pair<std::string, std::string>>& params)
    -> std::vector<std::string> {
    std::vector<std::string> types;
    for (const auto& [key, value] : params)
        if (underlying_of_type_key(key))
            types.push_back(value);
    return types;
}

/// States the type each appended underlying requires, empty when none is known.
void set_underlying_type(xml::record_writer& underlying,
                         const std::vector<std::string>& types,
                         std::size_t index) {
    underlying.set_text("Type", index < types.size() ? types[index] : std::string{});
}

}

auto read_scripted_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument> {
    if (trade_type == "ScriptedTrade") {
        auto r = scripted_instrument_projection_make_base(trade_type);
        if (const auto named = trade.child("AsianBasketOptionData")) {
            r.script_name = "AsianBasketOption";
            r.underlyings_json = json_array(vector_values(*named, "Underlyings"));
            r.parameters_json = make_params_json(own_text(*named, "PutCall"),
                                                 number_text(own_text(*named, "Notional")),
                                                 number_text(own_text(*named, "Strike")));
            return trading::domain::trade_instrument{std::move(r)};
        }
        if (const auto named = trade.child("AverageStrikeBasketOptionData")) {
            r.script_name = "AverageStrikeBasketOption";
            r.underlyings_json = json_array(vector_values(*named, "Underlyings"));
            r.parameters_json = make_params_json(
                own_text(*named, "PutCall"), number_text(own_text(*named, "Notional")), 0.0);
            return trading::domain::trade_instrument{std::move(r)};
        }
        if (const auto data = trade.child("ScriptedTradeData")) {
            if (const auto name = data->text("ScriptName"))
                r.script_name = *name;
            std::string npv;
            if (const auto script = data->child("Script")) {
                if (const auto code = script->text("Code"))
                    r.script_body = *code;
                if (const auto value = script->text("NPV"))
                    npv = *value;
            }
            if (const auto values = data->child("Data")) {
                std::string params = "{";
                bool first = true;
                // The script's NPV is a name the schema requires, and the
                // instrument's flat table has no column for it, so it rides in
                // the parameters the table does carry.
                if (!npv.empty()) {
                    params += "\"NPV\":\"" + npv + "\"";
                    first = false;
                }
                for (const auto& number : values->children("Number")) {
                    if (!first)
                        params += ",";
                    params += "\"" + number.text("Name").value_or("") + "\":\"" +
                              number.text("Value").value_or("") + "\"";
                    first = false;
                }
                if (!first)
                    r.parameters_json = params + "}";

                const auto indices = values->children("Index");
                if (!indices.empty()) {
                    std::vector<std::string> names;
                    for (const auto& index : indices)
                        names.push_back(index.text("Value").value_or(""));
                    r.underlyings_json = json_array(names);
                }
            }
        }
        return trading::domain::trade_instrument{std::move(r)};
    }

    if (trade_type == "DoubleDigitalOption") {
        auto r = scripted_instrument_projection_make_base(trade_type);
        r.script_name = "DoubleDigitalOption";
        if (const auto d = trade.child("DoubleDigitalOptionData")) {
            std::vector<std::string> names;
            std::vector<std::string> types;
            for (const auto* name : {"Underlying1", "Underlying2", "Underlying3", "Underlying4"}) {
                if (const auto u = d->child(name)) {
                    if (const auto n = u->text("Name")) {
                        names.push_back(*n);
                        types.push_back(u->text("Type").value_or(""));
                    }
                }
            }
            r.underlyings_json = json_array(names);
            const auto number = [&](std::string_view name) {
                return std::to_string(decimal_as_double(*d, name).value_or(0));
            };
            std::string params = "{\"BinaryPayout\":" + number("BinaryPayout") +
                                 ",\"BinaryLevel1\":" + number("BinaryLevel1") +
                                 ",\"BinaryLevel2\":" + number("BinaryLevel2") + ",\"Type1\":\"" +
                                 d->text("Type1").value_or("") + "\"" + ",\"Type2\":\"" +
                                 d->text("Type2").value_or("") + "\"" + ",\"Position\":\"" +
                                 d->spelling("Position").value_or("") + "\"" + ",\"Expiry\":\"" +
                                 d->text("Expiry").value_or("") + "\"" + ",\"Settlement\":\"" +
                                 d->text("Settlement").value_or("") + "\"" + ",\"PayCcy\":\"" +
                                 d->spelling("PayCcy").value_or("") + "\"";
            for (std::size_t i = 0; i < types.size(); ++i)
                if (!types[i].empty())
                    params += ",\"" + underlying_type_key(i) + "\":\"" + types[i] + "\"";
            r.parameters_json = params + "}";
        }
        return trading::domain::trade_instrument{std::move(r)};
    }

    if (trade_type == "PerformanceOption_01") {
        auto r = scripted_instrument_projection_make_base(trade_type);
        r.script_name = "PerformanceOption_01";
        if (const auto d = trade.child("PerformanceOption01Data")) {
            std::vector<std::string> names;
            std::vector<std::string> types;
            if (const auto underlyings = d->child("Underlyings"))
                for (const auto& u : underlyings->children("Underlying")) {
                    names.push_back(u.text("Name").value_or(""));
                    types.push_back(u.text("Type").value_or(""));
                }
            r.underlyings_json = json_array(names);

            const auto number = [&](std::string_view name) {
                return std::to_string(decimal_as_double(*d, name).value_or(0));
            };
            std::string params = "{\"NotionalAmount\":" + number("NotionalAmount") +
                                 ",\"ParticipationRate\":" + number("ParticipationRate") +
                                 ",\"Strike\":" + number("Strike") + ",\"ValuationDate\":\"" +
                                 d->text("ValuationDate").value_or("") + "\"" +
                                 ",\"SettlementDate\":\"" + d->text("SettlementDate").value_or("") +
                                 "\"" + ",\"Position\":\"" + d->spelling("Position").value_or("") +
                                 "\"" + ",\"PayCcy\":\"" + d->spelling("PayCcy").value_or("") +
                                 "\"";
            std::string joined;
            if (const auto prices = d->child("StrikePrices")) {
                for (const auto& strike : prices->decimals("StrikePrice")) {
                    if (!joined.empty())
                        joined += ",";
                    joined += strike.to_string();
                }
            }
            params += ",\"StrikePrices\":\"" + joined + "\"";
            for (std::size_t i = 0; i < types.size(); ++i)
                if (!types[i].empty())
                    params += ",\"" + underlying_type_key(i) + "\":\"" + types[i] + "\"";
            r.parameters_json = params + "}";
        }
        return trading::domain::trade_instrument{std::move(r)};
    }

    return std::nullopt;
}

auto write_scripted_instrument(xml::record_writer& trade,
                               std::string_view trade_type,
                               const trading::domain::trade_instrument& instrument,
                               const portfolio_reference_curves&) -> bool {
    const auto* r = std::get_if<scripted_instrument>(&instrument);
    if (r == nullptr)
        return false;

    if (trade_type == "ScriptedTrade") {
        auto data = trade.child("ScriptedTradeData");
        if (!r->script_name.empty())
            data.set_text("ScriptName", r->script_name);
        if (!r->script_body.empty())
            data.child("Script").set_text("Code", r->script_body);

        // The schema requires the data element even when the row states no
        // parameter, so it is stated before anything fills it.
        auto values = data.child("Data");
        const auto numbers = parse_json_object(r->parameters_json);
        for (const auto& [name, value] : numbers) {
            if (name == "NPV") {
                data.child("Script").set_text("NPV", value);
                continue;
            }
            auto number = values.append("Number");
            number.set_text("Name", name);
            number.set_text("Value", value);
        }
        const auto indices = parse_json_string_array(r->underlyings_json);
        for (const auto& value : indices) {
            auto index = values.append("Index");
            index.set_text("Name", "Underlying");
            index.set_text("Value", value);
        }
        return true;
    }

    if (trade_type == "DoubleDigitalOption") {
        auto d = trade.child("DoubleDigitalOptionData");
        const auto params = parse_json_object(r->parameters_json);
        for (const auto& [key, value] : params) {
            if (key == "Type1" || key == "Type2" || key == "Expiry" || key == "Settlement")
                d.set_text(key, value);
            else if (key == "Position")
                d.set_spelling("Position", value, "long short");
            else if (key == "PayCcy")
                d.set_spelling("PayCcy", value, "currency");
            else if (underlying_of_type_key(key))
                continue;
            else
                set_decimal_from_double(d, key, std::stod(value));
        }
        const auto names = parse_json_string_array(r->underlyings_json);
        const auto types = underlying_types_of(params);
        for (std::size_t i = 0; i < names.size() && i < 4; ++i) {
            auto underlying = d.child("Underlying" + std::to_string(i + 1));
            underlying.set_text("Name", names[i]);
            set_underlying_type(underlying, types, i);
        }
        return true;
    }

    if (trade_type == "PerformanceOption_01") {
        auto d = trade.child("PerformanceOption01Data");
        std::vector<std::string> strike_prices;
        const auto params = parse_json_object(r->parameters_json);
        for (const auto& [key, value] : params) {
            if (key == "Position")
                d.set_spelling("Position", value, "long short");
            else if (key == "PayCcy")
                d.set_spelling("PayCcy", value, "currency");
            else if (key == "ValuationDate" || key == "SettlementDate")
                d.set_text(key, value);
            else if (key == "StrikePrices") {
                std::size_t start = 0;
                while (start < value.size()) {
                    const auto comma = value.find(',', start);
                    const auto length =
                        comma == std::string::npos ? value.size() - start : comma - start;
                    const auto token = value.substr(start, length);
                    if (!token.empty())
                        strike_prices.push_back(token);
                    if (comma == std::string::npos)
                        break;
                    start = comma + 1;
                }
            } else if (underlying_of_type_key(key))
                continue;
            else
                set_decimal_from_double(d, key, std::stod(value));
        }
        if (!strike_prices.empty()) {
            auto prices = d.child("StrikePrices");
            for (const auto& price : strike_prices) {
                if (const auto parsed = ores::utility::decimal::decimal::from_string(price))
                    prices.append_decimal("StrikePrice", *parsed);
            }
        }
        const auto names = parse_json_string_array(r->underlyings_json);
        if (!names.empty()) {
            const auto types = underlying_types_of(params);
            auto underlyings = d.child("Underlyings");
            for (std::size_t i = 0; i < names.size(); ++i) {
                auto underlying = underlyings.append("Underlying");
                underlying.set_text("Name", names[i]);
                set_underlying_type(underlying, types, i);
            }
        }
        return true;
    }

    return false;
}

}
