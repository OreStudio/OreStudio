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

// This translation unit is intentionally isolated from parse_trade_instrument.cpp.
//
// MSVC C1202 ("recursive type or function dependency context too complex") fires
// when rfl::json::read<> is instantiated for too many types in the same TU: the
// accumulated rfl::StringLiteral<N> field-name types fill MSVC's internal template
// dependency graph. Splitting the assembled bond container into this dedicated TU
// gives MSVC a clean slate; the flat/FX/equity types in parse_trade_instrument.cpp
// never enter this compilation context.

#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.qt.headless/IInstrumentFormPopulator.hpp"
#include "ores.trading.api/domain/bond_instrument_data.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "parse_bond_impl.hpp"
#include <optional>
#include <span>

namespace {

using namespace ores::logging;
namespace td = ores::trading::domain;

inline std::string_view logger_name = "ores.qt.parse_bond_instruments";
[[nodiscard]] static auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

template <typename T>
static std::optional<T> try_parse(const std::string& raw) {
    const std::span<const std::byte> bytes(reinterpret_cast<const std::byte*>(raw.data()),
                                           raw.size());
    auto r = ores::nats::default_wire_codec().decode<T>(bytes);
    if (!r) {
        BOOST_LOG_SEV(lg(), error)
            << "parse_bond_instrument: deserialise failed: " << r.error().what();
        return std::nullopt;
    }
    return std::move(*r);
}

// The container is the bond alternative of trade_instrument: its members (the
// slim header row, the issue row and the engaged fact rows) sit under the
// response's "instrument" key, one nested object per member.
struct bond_wrapper {
    td::bond_instrument_data instrument;
};

} // namespace

namespace ores::qt::internal {

bool parse_bond_instrument(const std::string& raw, ores::qt::IInstrumentFormPopulator& pop) {
    using namespace ores::logging;

    BOOST_LOG_SEV(lg(), debug) << "parse_bond_instrument: reading bond_instrument_data";
    auto r = try_parse<bond_wrapper>(raw);
    if (!r)
        return false;
    pop.populate(r->instrument);
    return true;
}

} // namespace ores::qt::internal
