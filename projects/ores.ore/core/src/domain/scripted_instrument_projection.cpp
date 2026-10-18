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
auto make_base(std::string_view trade_type_code) -> scripted_instrument {
    scripted_instrument r;
    r.identity.trade_type_code = std::string(trade_type_code);
    r.audit.modified_by = "ores";
    r.audit.performed_by = "ores";
    r.audit.change_reason_code = "system.external_data_import";
    r.audit.change_commentary = "Imported from ORE XML";
    return r;
}

auto json_escape(std::string_view text) -> std::string {
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
        out += "\"" + json_escape(values[i]) + "\"";
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
    return "{\"PutCall\":\"" + std::string(put_call) + "\",\"Notional\":" +
           std::to_string(notional) + ",\"Strike\":" + std::to_string(strike) + "}";
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

auto vector_values(const xml::record_reader& record, std::string_view name) -> std::vector<std::string> {
    const auto child = record.child(name);
    if (!child)
        return {};
    return child->values("Value");
}

}

auto read_scripted_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument> {
    if (trade_type == "ScriptedTrade") {
        auto r = make_base(trade_type);
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
            r.parameters_json = make_params_json(own_text(*named, "PutCall"),
                                                 number_text(own_text(*named, "Notional")),
                                                 0.0);
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
        auto r = make_base(trade_type);
        r.script_name = "DoubleDigitalOption";
        if (const auto d = trade.child("DoubleDigitalOptionData")) {
            std::vector<std::string> names;
            for (const auto* name : {"Underlying1", "Underlying2", "Underlying3", "Underlying4"}) {
                if (const auto u = d->child(name)) {
                    if (const auto n = u->text("Name"))
                        names.push_back(*n);
                }
            }
            r.underlyings_json = json_array(names);
            r.parameters_json =
                "{\"BinaryPayout\":" +
                std::to_string(number_text(own_text(*d, "BinaryPayout"))) +
                ",\"BinaryLevel1\":" +
                std::to_string(number_text(own_text(*d, "BinaryLevel1"))) +
                ",\"BinaryLevel2\":" +
                std::to_string(number_text(own_text(*d, "BinaryLevel2"))) + ",\"Type1\":\"" +
                own_text(*d, "Type1") + "\"" + ",\"Type2\":\"" + own_text(*d, "Type2") + "\"" +
                ",\"Position\":\"" + d->spelling("Position").value_or("") + "\"" +
                ",\"Expiry\":\"" + own_text(*d, "Expiry") + "\"" + ",\"Settlement\":\"" +
                own_text(*d, "Settlement") + "\"" + ",\"PayCcy\":\"" +
                d->spelling("PayCcy").value_or("") + "\"}";
        }
        return trading::domain::trade_instrument{std::move(r)};
    }

    if (trade_type == "PerformanceOption_01") {
        auto r = make_base(trade_type);
        r.script_name = "PerformanceOption_01";
        if (const auto d = trade.child("PerformanceOption01Data")) {
            std::vector<std::string> names;
            if (const auto underlyings = d->child("Underlyings"))
                for (const auto& u : underlyings->children("Underlying"))
                    names.push_back(u.text("Name").value_or(""));
            r.underlyings_json = json_array(names);

            std::string params = "{\"NotionalAmount\":" +
                                 std::to_string(number_text(own_text(*d, "NotionalAmount"))) +
                                 ",\"ParticipationRate\":" +
                                 std::to_string(number_text(own_text(*d, "ParticipationRate"))) +
                                 ",\"Strike\":" + std::to_string(number_text(own_text(*d, "Strike"))) +
                                 ",\"ValuationDate\":\"" + own_text(*d, "ValuationDate") + "\"" +
                                 ",\"SettlementDate\":\"" + own_text(*d, "SettlementDate") + "\"" +
                                 ",\"Position\":\"" + d->spelling("Position").value_or("") + "\"" +
                                 ",\"PayCcy\":\"" + d->spelling("PayCcy").value_or("") + "\"";
            std::vector<std::string> strikes;
            if (const auto prices = d->child("StrikePrices"))
                strikes = prices->values("StrikePrice");
            std::string joined;
            for (const auto& strike : strikes) {
                if (!joined.empty())
                    joined += ",";
                joined += strike;
            }
            params += ",\"StrikePrices\":\"" + joined + "\"}";
            r.parameters_json = params;
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

        const auto numbers = parse_json_object(r->parameters_json);
        for (const auto& [name, value] : numbers) {
            if (name == "NPV") {
                data.child("Script").set_text("NPV", value);
                continue;
            }
            auto number = data.child("Data").append("Number");
            number.set_text("Name", name);
            number.set_text("Value", value);
        }
        const auto indices = parse_json_string_array(r->underlyings_json);
        if (!indices.empty()) {
            auto values = data.child("Data");
            for (const auto& value : indices) {
                auto index = values.append("Index");
                index.set_text("Name", "Underlying");
                index.set_text("Value", value);
            }
        }
        return true;
    }

    if (trade_type == "DoubleDigitalOption") {
        auto d = trade.child("DoubleDigitalOptionData");
        const auto params = parse_json_object(r->parameters_json);
        for (const auto& [key, value] : params) {
            if (key == "Type1" || key == "Type2")
                d.child(key).set_own_text(value);
            else if (key == "Position")
                d.set_spelling("Position", value, "long short");
            else if (key == "Expiry" || key == "Settlement")
                d.child(key).set_own_text(value);
            else if (key == "PayCcy")
                d.set_spelling("PayCcy", value, "currency");
            else
                d.child(key).set_own_text(value);
        }
        const auto names = parse_json_string_array(r->underlyings_json);
        for (std::size_t i = 0; i < names.size() && i < 4; ++i) {
            const auto name = "Underlying" + std::to_string(i + 1);
            d.child(name).set_text("Name", names[i]);
        }
        return true;
    }

    if (trade_type == "PerformanceOption_01") {
        auto d = trade.child("PerformanceOption01Data");
        std::vector<std::string> strike_prices;
        for (const auto& [key, value] : parse_json_object(r->parameters_json)) {
            if (key == "Position")
                d.set_spelling("Position", value, "long short");
            else if (key == "PayCcy")
                d.set_spelling("PayCcy", value, "currency");
            else if (key == "StrikePrices") {
                std::size_t start = 0;
                while (start < value.size()) {
                    const auto comma = value.find(',', start);
                    const auto length = comma == std::string::npos ? value.size() - start : comma - start;
                    const auto token = value.substr(start, length);
                    if (!token.empty())
                        strike_prices.push_back(token);
                    if (comma == std::string::npos)
                        break;
                    start = comma + 1;
                }
            } else
                d.child(key).set_own_text(value);
        }
        if (!strike_prices.empty()) {
            auto prices = d.child("StrikePrices");
            for (const auto& price : strike_prices)
                prices.append_value("StrikePrice", price);
        }
        const auto names = parse_json_string_array(r->underlyings_json);
        if (!names.empty()) {
            auto underlyings = d.child("Underlyings");
            for (const auto& name : names)
                underlyings.append("Underlying").set_text("Name", name);
        }
        return true;
    }

    return false;
}

auto instrument_projections() -> const std::vector<instrument_projection>& {
    static const std::vector<instrument_projection> all = {
        {"scripted", &read_scripted_instrument, &write_scripted_instrument},
    };
    return all;
}

}
