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
#ifndef ORES_TRADING_API_DOMAIN_INSTRUMENT_BATCH_MAPPER_HPP
#define ORES_TRADING_API_DOMAIN_INSTRUMENT_BATCH_MAPPER_HPP

#include "ores.trading.api/domain/instrument_batch.hpp"
#include "ores.trading.api/domain/trade_instrument.hpp"
#include "ores.trading.api/export.hpp"
#include <boost/uuid/uuid.hpp>

namespace ores::trading::domain {

/**
 * @brief Adds an in-memory instrument to a batch.
 *
 * The reverse of rebuild_instrument, for a caller that holds the assembled
 * carrier rather than the parts: the import path and the tests build a
 * trade_instrument and need the wire shape.
 */
ORES_TRADING_API_EXPORT void append_instrument(instrument_batch& batch,
                                               const trade_instrument& instrument);

/**
 * @brief The instrument a trade's batch rows state, or a monostate instrument
 * when the batch holds none.
 *
 * The batch is keyed by trade id, so this is the join the export reader makes
 * once per trade. The carrier's part is chosen by which array holds the trade,
 * and its children are every row of theirs that carries the same id.
 */
ORES_TRADING_API_EXPORT trade_instrument rebuild_instrument(const instrument_batch& batch,
                                                            boost::uuids::uuid trade_id);

}

#endif
