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
#ifndef ORES_SYNTHETIC_API_FEEDS_PRODUCER_SUBJECT_HPP
#define ORES_SYNTHETIC_API_FEEDS_PRODUCER_SUBJECT_HPP

#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.synthetic.api/domain/binding_mode.hpp"
#include <cctype>
#include <string>
#include <string_view>

namespace ores::synthetic::feed {

/**
 * @brief Build every producer's publish subject from source_name and
 * binding_mode. '.' is kept (it is the NATS hierarchy separator and source
 * names are dotted), but any character that is not a safe subject token —
 * whitespace, wildcards ('*', '>'), or non-alphanumerics other than '.', '_',
 * '-' — is replaced with '_' so a stray value cannot produce surprise routing
 * or a publish error.
 *
 * A sandboxed feed publishes under "synthetic.v1.ops.sandbox_tick." rather than
 * "synthetic.v1.ops.tick.<source>", which the marketdata ingest loop never
 * subscribes to, so its ticks cannot be stored whatever bindings exist. This
 * is the one subject builder for every asset class; a producer supplies only
 * its source_name and binding_mode.
 */
inline std::string producer_subject(std::string_view source_name,
                                    ores::synthetic::domain::binding_mode binding_mode) {
    std::string token;
    token.reserve(source_name.size());
    for (unsigned char c : source_name) {
        const bool safe = std::isalnum(c) || c == '.' || c == '_' || c == '-';
        token += safe ? static_cast<char>(c) : '_';
    }
    const bool sandboxed = binding_mode == ores::synthetic::domain::binding_mode::sandboxed;
    return sandboxed ?
               std::string(ores::marketdata::domain::synthetic_sandbox_tick_subject_prefix) +
                   token :
               ores::marketdata::domain::synthetic_tick_subject(token);
}

}

#endif
