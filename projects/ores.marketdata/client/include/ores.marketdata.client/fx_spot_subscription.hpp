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
#ifndef ORES_MARKETDATA_CLIENT_FX_SPOT_SUBSCRIPTION_HPP
#define ORES_MARKETDATA_CLIENT_FX_SPOT_SUBSCRIPTION_HPP

#include "ores.marketdata.api/domain/fx_spot_tick.hpp"
#include "ores.marketdata.client/export.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include <functional>
#include <string>

namespace ores::marketdata::client {

/**
 * @brief RAII subscription to a single FX spot tick stream.
 *
 * Translates an ORE key (e.g. "FX/RATE/EUR/USD") into the canonical NATS
 * fan-out subject ("marketdata.v1.tick.fx.rate.eur.usd") and subscribes to
 * it. Each arriving message is deserialised with rfl::json and delivered to
 * the caller-supplied handler. Malformed messages are logged and silently
 * dropped.
 *
 * The subscription is torn down automatically when this object is destroyed.
 * Move-only — the underlying NATS subscription cannot be shared.
 *
 * @note The handler is invoked on a NATS delivery thread, not the Qt GUI thread.
 *       Callers that update Qt widgets must marshal back via
 *       QMetaObject::invokeMethod or Qt::QueuedConnection.
 *
 * @pre The @p nats client must be connected and must outlive this object.
 */
class ORES_MARKETDATA_CLIENT_EXPORT fx_spot_subscription {
public:
    /**
     * @brief Callback invoked on each well-formed tick.
     */
    using handler = std::function<void(const domain::fx_spot_tick&)>;

    /**
     * @brief Subscribe to tick updates for an ORE key.
     *
     * @param nats      Connected NATS client to subscribe on.
     * @param ore_key   ORE canonical key, e.g. "FX/RATE/EUR/USD".
     *                  Converted internally to "marketdata.v1.tick.fx.rate.eur.usd".
     * @param on_tick   Invoked on the NATS delivery thread for each valid tick.
     */
    fx_spot_subscription(ores::nats::service::client& nats, std::string ore_key, handler on_tick);

    ~fx_spot_subscription() = default;

    fx_spot_subscription(const fx_spot_subscription&) = delete;
    fx_spot_subscription& operator=(const fx_spot_subscription&) = delete;

    fx_spot_subscription(fx_spot_subscription&&) = default;
    fx_spot_subscription& operator=(fx_spot_subscription&&) = default;

private:
    ores::nats::service::subscription sub_;
};

}

#endif
