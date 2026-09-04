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
 * Translates an ORE key (e.g. "FX/RATE/EUR/USD") plus the consuming
 * (tenant, workspace, party) context into the canonical NATS fan-out
 * subject ("marketdata.v1.tick.<tenant>.<workspace>.<party>.fx.rate.eur.usd")
 * and subscribes to it. Each arriving message is deserialised with the
 * process-wide
 * default_wire_codec() (matches whatever ORES_NATS_WIRE_FORMAT the
 * publisher used) and delivered to the caller-supplied handler. Malformed
 * messages are logged and reported via @p on_error, if supplied — a
 * consistently-failing stream (e.g. a wire-format mismatch) is otherwise
 * invisible in the UI: ticks silently never arrive, with nothing to tell
 * the user why.
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
     * @brief Callback invoked (on the NATS delivery thread, same as @ref
     * handler) when a tick fails to deserialise. Not rate-limited: under
     * normal operation this path should never fire, since the codec
     * matches the publisher's own wire format -- if it does fire
     * repeatedly, that is itself the signal worth surfacing (e.g. a wire
     * format mismatch, or a genuinely corrupt stream), not something to
     * suppress.
     */
    using error_handler = std::function<void(const std::string&)>;

    /**
     * @brief Subscribe to tick updates for an ORE key.
     *
     * @param nats         Connected NATS client to subscribe on.
     * @param ore_key      ORE canonical key, e.g. "FX/RATE/EUR/USD".
     * @param tenant_id    Tenant UUID string; included in the NATS subject so
     *                     the subscription matches the tenant-scoped publish
     *                     path used by the ingest loop.
     * @param workspace_id Workspace UUID string; the stream the ingest loop
     *                     republishes on is per (tenant, workspace, party,
     *                     ore_key). Consumers of the Live market pass the
     *                     Live sentinel (aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa);
     *                     scenario workspaces will pass their own id.
     * @param party_id     Party UUID string; the party whose per-party stream
     *                     this subscription follows.
     * @param on_tick      Invoked on the NATS delivery thread for each valid tick.
     * @param on_error     Optional; invoked on the NATS delivery thread for each
     *                     tick that fails to deserialise. Failures are always
     *                     logged regardless of whether this is supplied.
     */
    fx_spot_subscription(ores::nats::service::client& nats,
                         std::string ore_key,
                         std::string tenant_id,
                         std::string workspace_id,
                         std::string party_id,
                         handler on_tick,
                         error_handler on_error = {});

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
