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
#include "ores.ore/domain/scripted_instrument_mapper.hpp"

namespace ores::ore::domain {

using namespace ores::logging;
using ores::trading::domain::scripted_instrument;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

namespace {

scripted_instrument make_base(const std::string& trade_type_code) {
    scripted_instrument r;
    r.trade_type_code = trade_type_code;
    r.modified_by = "ores";
    r.performed_by = "ores";
    r.change_reason_code = "system.external_data_import";
    r.change_commentary = "Imported from ORE XML";
    return r;
}

// Build a JSON array of index strings from stFreeStyleIndexVector.
std::string index_vector_to_json(const stFreeStyleIndexVector& v) {
    std::string json = "[";
    bool first = true;
    for (const auto& val : v.Value) {
        if (!first) json += ",";
        // Escape backslashes and double-quotes
        const std::string s = std::string(val);
        std::string esc;
        for (char c : s) {
            if (c == '"')       esc += "\\\"";
            else if (c == '\\') esc += "\\\\";
            else                esc += c;
        }
        json += "\"" + esc + "\"";
        first = false;
    }
    json += "]";
    return json;
}

// Build a minimal JSON parameters object from key scalar fields of a
// named scripted product.
std::string make_params_json(const std::string& put_call,
                             float notional, float strike) {
    return "{\"PutCall\":\"" + put_call + "\","
           "\"Notional\":" + std::to_string(notional) + ","
           "\"Strike\":" + std::to_string(strike) + "}";
}

// Parse a JSON array of strings: ["val1","val2",...] -> vector of strings.
std::vector<std::string> parse_json_string_array(const std::string& json) {
    std::vector<std::string> result;
    std::size_t i = 0;
    while (i < json.size() && json[i] != '[') ++i;
    if (i >= json.size()) return result;
    ++i; // skip '['
    while (i < json.size()) {
        while (i < json.size() &&
               (json[i] == ' ' || json[i] == ',' || json[i] == '\t')) ++i;
        if (i >= json.size() || json[i] == ']') break;
        if (json[i] != '"') { ++i; continue; }
        ++i; // skip opening quote
        std::string val;
        while (i < json.size() && json[i] != '"') {
            if (json[i] == '\\' && i + 1 < json.size()) {
                ++i;
                if (json[i] == '"')       val += '"';
                else if (json[i] == '\\') val += '\\';
                else                      val += json[i];
            } else {
                val += json[i];
            }
            ++i;
        }
        ++i; // skip closing quote
        result.push_back(std::move(val));
    }
    return result;
}

// Parse a simple JSON object: {"Key":"strval",...} or {"Key":numval,...}.
// Values are returned as strings; numeric values are kept as their JSON text.
std::vector<std::pair<std::string,std::string>>
parse_json_object(const std::string& json) {
    std::vector<std::pair<std::string,std::string>> result;
    std::size_t i = 0;
    while (i < json.size() && json[i] != '{') ++i;
    if (i >= json.size()) return result;
    ++i; // skip '{'
    while (i < json.size()) {
        while (i < json.size() &&
               (json[i] == ' ' || json[i] == ',' || json[i] == '\t')) ++i;
        if (i >= json.size() || json[i] == '}') break;
        if (json[i] != '"') { ++i; continue; }
        ++i; // skip opening quote of key
        std::string key;
        while (i < json.size() && json[i] != '"') { key += json[i]; ++i; }
        ++i; // skip closing quote of key
        while (i < json.size() && json[i] != ':') ++i;
        ++i; // skip ':'
        while (i < json.size() && (json[i] == ' ' || json[i] == '\t')) ++i;
        std::string val;
        if (i < json.size() && json[i] == '"') {
            ++i; // skip opening quote of string value
            while (i < json.size() && json[i] != '"') {
                if (json[i] == '\\' && i + 1 < json.size()) {
                    ++i; val += json[i];
                } else {
                    val += json[i];
                }
                ++i;
            }
            ++i; // skip closing quote
        } else {
            // numeric or other bare value – read until ',' or '}'
            while (i < json.size() && json[i] != ',' && json[i] != '}')
                val += json[i++];
        }
        result.emplace_back(std::move(key), std::move(val));
    }
    return result;
}

} // namespace

// ---------------------------------------------------------------------------
// Forward: ScriptedTrade
// ---------------------------------------------------------------------------

scripted_mapping_result scripted_instrument_mapper::forward_scripted_trade(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping ScriptedTrade: "
                               << std::string(t.id);
    scripted_mapping_result result;
    result.instrument = make_base("ScriptedTrade");

    // Named scripted product: AsianBasketOptionData
    if (t.AsianBasketOptionData) {
        const auto& d = *t.AsianBasketOptionData;
        result.instrument.script_name = "AsianBasketOption";
        result.instrument.underlyings_json = index_vector_to_json(d.Underlyings);
        result.instrument.parameters_json = make_params_json(
            std::string(d.PutCall),
            static_cast<float>(d.Notional),
            static_cast<float>(d.Strike));
        return result;
    }

    // Named scripted product: AverageStrikeBasketOptionData
    if (t.AverageStrikeBasketOptionData) {
        const auto& d = *t.AverageStrikeBasketOptionData;
        result.instrument.script_name = "AverageStrikeBasketOption";
        result.instrument.underlyings_json = index_vector_to_json(d.Underlyings);
        result.instrument.parameters_json = make_params_json(
            std::string(d.PutCall),
            static_cast<float>(d.Notional),
            0.0f);
        return result;
    }

    // Generic ScriptedTradeData
    if (t.ScriptedTradeData) {
        const auto& d = *t.ScriptedTradeData;
        if (d.ScriptName)
            result.instrument.script_name = std::string(*d.ScriptName);
        if (d.Script)
            result.instrument.script_body = std::string(d.Script->Code);
        // Encode Number parameters into parameters_json
        if (!d.Data.Number.empty()) {
            std::string params = "{";
            bool first = true;
            for (const auto& num : d.Data.Number) {
                if (!first) params += ",";
                const std::string val =
                    num.Value ? std::string(*num.Value) : "";
                params += "\"" + std::string(num.Name) + "\":\""
                        + val + "\"";
                first = false;
            }
            params += "}";
            result.instrument.parameters_json = params;
        }
        // Encode Index entries into underlyings_json
        if (!d.Data.Index.empty()) {
            std::string underlyings = "[";
            bool first = true;
            for (const auto& idx : d.Data.Index) {
                if (!first) underlyings += ",";
                const std::string val =
                    idx.Value ? std::string(*idx.Value) : "";
                underlyings += "\"" + val + "\"";
                first = false;
            }
            underlyings += "]";
            result.instrument.underlyings_json = underlyings;
        }
        return result;
    }

    return result;
}

// ---------------------------------------------------------------------------
// Reverse: ScriptedTrade
// ---------------------------------------------------------------------------

trade scripted_instrument_mapper::reverse_scripted_trade(
        const scripted_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping ScriptedTrade";
    trade t;
    t.TradeType = oreTradeType::ScriptedTrade;

    scriptedTradeData d;
    if (!instr.script_name.empty()) {
        scriptedTradeData_ScriptName_t sn;
        static_cast<std::string&>(sn) = instr.script_name;
        d.ScriptName = std::move(sn);
    }

    // Reconstruct inline script body if present.
    if (!instr.script_body.empty()) {
        ore_script s;
        static_cast<std::string&>(s.Code) = instr.script_body;
        d.Script = std::move(s);
    }

    // Reconstruct Index entries from underlyings_json (JSON array of strings).
    // Synthetic names "Underlying_N" are used because the serialised array form
    // does not preserve the original parameter names.
    if (!instr.underlyings_json.empty()) {
        const auto vals = parse_json_string_array(instr.underlyings_json);
        for (std::size_t idx = 0; idx < vals.size(); ++idx) {
            scriptedTradeData_Data_t_Index_t entry;
            static_cast<std::string&>(entry.Name) =
                "Underlying_" + std::to_string(idx);
            scriptedTradeData_Data_t_Index_t_Value_t v;
            static_cast<std::string&>(v) = vals[idx];
            entry.Value = std::move(v);
            d.Data.Index.push_back(std::move(entry));
        }
    }

    // Reconstruct Number entries from parameters_json (JSON object).
    if (!instr.parameters_json.empty()) {
        for (const auto& [key, val] : parse_json_object(instr.parameters_json)) {
            scriptedTradeData_Data_t_Number_t entry;
            static_cast<std::string&>(entry.Name) = key;
            scriptedTradeData_Data_t_Number_t_Value_t v;
            static_cast<std::string&>(v) = val;
            entry.Value = std::move(v);
            d.Data.Number.push_back(std::move(entry));
        }
    }

    t.ScriptedTradeData = std::move(d);
    return t;
}

}
