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
#include "ores.marketdata.client/fx_spot_subscription.hpp"
#include "ores.marketdata.client/detail/subject_helpers.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include <rfl/json.hpp>

namespace ores::marketdata::client {

namespace {

using namespace ores::logging;

inline static std::string_view logger_name =
    "ores.marketdata.client.fx_spot_subscription";

[[nodiscard]] auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

} // namespace

fx_spot_subscription::fx_spot_subscription(ores::nats::service::client& nats,
                                           std::string ore_key,
                                           handler on_tick)
    : sub_(nats.subscribe(
          detail::ore_key_to_subject(ore_key),
          [on_tick = std::move(on_tick)](ores::nats::message msg) {
              auto tick = rfl::json::read<ores::marketdata::domain::fx_spot_tick>(
                  ores::nats::as_string_view(msg.data));
              if (tick) {
                  on_tick(*tick);
              } else {
                  using namespace ores::logging;
                  BOOST_LOG_SEV(lg(), warn)
                      << "Failed to deserialise fx_spot_tick: "
                      << tick.error().what();
              }
          })) {}

} // namespace ores::marketdata::client
