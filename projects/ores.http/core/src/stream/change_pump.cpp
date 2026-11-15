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
#include "ores.http.core/stream/change_pump.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <format>

namespace ores::http::stream {

using namespace ores::logging;

namespace {

inline auto& pump_lg() {
    static auto instance = ores::logging::make_logger("ores.http.stream.change_pump");
    return instance;
}

/// Escapes the few characters JSON cannot carry as they are.
std::string json_string(std::string_view value) {
    std::string out = "\"";
    for (const auto ch : value) {
        if (ch == '"' || ch == '\\')
            out += '\\';
        out += ch;
    }
    out += '"';
    return out;
}

}

std::string instant_text(std::chrono::system_clock::time_point at) {
    return std::format("{:%FT%T}Z", std::chrono::floor<std::chrono::milliseconds>(at));
}

std::string frame_text(const coalesced_frame& frame, std::string_view at) {
    auto text = std::string("event: entity-changed\n");
    text += "data: {\"component\":" + json_string(frame.component);
    text += ",\"entity\":" + json_string(frame.entity);
    text += ",\"at\":" + json_string(std::string(at));
    text += ",\"tenant\":" + json_string(frame.tenant);
    text += ",\"party\":" + json_string(frame.party);
    text += "}\n\n";
    return text;
}

std::size_t change_pump::pump() {
    std::size_t published = 0;
    const auto at = instant_text(std::chrono::system_clock::now());
    for (const auto& frame : coalescer_.drain()) {
        hub_.publish(
            frame.tenant, frame.party, frame.component + '.' + frame.entity, frame_text(frame, at));
        ++published;
    }
    if (published > 0)
        BOOST_LOG_SEV(pump_lg(), debug)
            << "Published " << published << " coalesced frame(s) to every open stream";
    return published;
}

}
