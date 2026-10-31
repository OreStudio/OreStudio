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

namespace ores::http::stream {

namespace {

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

std::string frame_text(const coalesced_frame& frame) {
    auto text = std::string("event: ") + frame.entity + "\n";
    text += "data: {\"tenant\":" + json_string(frame.tenant);
    text += ",\"party\":" + json_string(frame.party);
    text += ",\"entity\":" + json_string(frame.entity);
    text += ",\"ids\":[";
    for (std::size_t index = 0; index < frame.ids.size(); ++index) {
        if (index > 0)
            text += ',';
        text += json_string(frame.ids[index]);
    }
    text += "]}\n\n";
    return text;
}

std::size_t change_pump::pump() {
    std::size_t published = 0;
    for (const auto& frame : coalescer_.drain()) {
        hub_.publish(frame_text(frame));
        ++published;
    }
    return published;
}

}
