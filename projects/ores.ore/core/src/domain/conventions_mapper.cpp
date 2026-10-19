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
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.ore.core/xml/emitter.hpp"
#include "ores.ore.core/xml/record.hpp"
#include <format>
#include <map>
#include <stdexcept>

namespace ores::ore::domain {

namespace xg = xml::generated::conventions;
using xml::record_reader;

conventions conventions_mapper::read(std::string_view text, std::string_view origin) {
    conventions record;
    const auto bound = xml::binder::bind(xg::conventions_document(), text, &record);
    if (!bound) {
        const auto where = origin.empty() ? std::string("conventions") : std::string(origin);
        throw std::invalid_argument(
            std::format("{}: line {}: {}", where, bound.error().line, bound.error().message));
    }
    return record;
}

std::string conventions_mapper::write(const conventions& v) {
    std::string out;
    const auto emitted = xml::emitter::emit(xg::conventions_document(), &v, out);
    if (!emitted)
        throw std::runtime_error(emitted.error().message);
    return out;
}

std::map<std::string, std::size_t> conventions_mapper::unmodelled(const conventions& v) {
    // Every category the document carries that this mapper does not model. A
    // skip that is counted is a gap a caller can read; a skip that is silent is
    // a document losing content and saying nothing.
    std::map<std::string, std::size_t> r;
    const auto count = [&r](std::string_view name, std::size_t n) {
        if (n != 0)
            r.emplace(std::string(name), n);
    };
    const record_reader root(*xg::conventions_document().root, &v);
    count("FxOptionTimeWeighting", root.children("FxOptionTimeWeighting").size());

    std::size_t rebasing_events = 0;
    for (const auto& x : root.children("ZeroInflationIndex"))
        if (x.has("RebasingEvents"))
            ++rebasing_events;
    count("ZeroInflationIndex.RebasingEvents", rebasing_events);
    return r;
}

}
