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
#ifndef ORES_REPORTING_SERVICE_APP_CUT_AUTHORISATION_RECONCILER_HPP
#define ORES_REPORTING_SERVICE_APP_CUT_AUTHORISATION_RECONCILER_HPP

#include "ores.reporting.core/service/cut_authorisation_applier.hpp"
#include <boost/asio/awaitable.hpp>
#include <cstdint>
#include <memory>

namespace ores::reporting::service::app {

/**
 * @brief Applies the decided cut authorisations on a clock, so a missed event
 * converges.
 *
 * The request's change event is the nudge and not the instruction: it reaches
 * this service only when the service is up, the subscriber is registered and the
 * broker keeps the message. A decision taken in between would wait for the next
 * decision on the tenant. This sweep reads the store instead of listening to it,
 * so a decision nobody announced is applied within the interval, and the run is
 * the same idempotent reconciliation the event runs.
 *
 * Runs as an async coroutine co-spawned beside the service's other loops.
 */
class cut_authorisation_reconciler {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance = ores::logging::make_logger(
            "ores.reporting.service.app.cut_authorisation_reconciler");
        return instance;
    }

public:
    cut_authorisation_reconciler(std::uint32_t interval_seconds,
                                 std::shared_ptr<service::cut_authorisation_applier> applier);

    boost::asio::awaitable<void> run();

    /**
     * @brief One reconciliation, with a failure logged rather than thrown.
     *
     * Public so a test can drive one pass without a clock.
     */
    int sweep();

private:
    std::uint32_t interval_seconds_;
    std::shared_ptr<service::cut_authorisation_applier> applier_;
};

}

#endif
