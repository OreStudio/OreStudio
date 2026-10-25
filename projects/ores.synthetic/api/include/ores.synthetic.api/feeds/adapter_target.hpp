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
#ifndef ORES_SYNTHETIC_API_FEEDS_ADAPTER_TARGET_HPP
#define ORES_SYNTHETIC_API_FEEDS_ADAPTER_TARGET_HPP

#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.synthetic.api/feeds/adapter_faults.hpp"
#include <memory>
#include <string>

namespace ores::synthetic::feed {

/**
 * @brief The vendor id a generator quotes and the subject it publishes the quote on.
 *
 * A generator with a target publishes an @c adapter_tick of that id on the subject. A
 * generator without one publishes the old @c market_tick on its producer subject.
 */
struct adapter_target final {
    ores::marketdata::messaging::vendor_id id;
    std::string subject;
    /// The faults in force. Empty means none is ever injected.
    std::shared_ptr<const fault_state> faults;
};

/**
 * @brief One pillar of a curve and the adapter id it quotes.
 *
 * A curve publishes every pillar it holds, and each pillar is its own vendor id on its own
 * subject, so a curve that serves an adapter carries one of these for each of its pillars. The
 * pillar is named as the curve template names it: the tenor it starts at and the tenor it ends at.
 */
struct ir_pillar_quote_target final {
    std::string start_tenor_code;
    std::string end_tenor_code;
    adapter_target target;
};

}

#endif
