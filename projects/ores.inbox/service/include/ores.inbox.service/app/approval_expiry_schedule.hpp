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
#ifndef ORES_INBOX_SERVICE_APP_APPROVAL_EXPIRY_SCHEDULE_HPP
#define ORES_INBOX_SERVICE_APP_APPROVAL_EXPIRY_SCHEDULE_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <boost/asio/awaitable.hpp>
#include <expected>
#include <string>
#include <string_view>

namespace ores::inbox::service::app {

/**
 * @brief Puts the recurring approval expiry sweep into the scheduler.
 *
 * The component declares that expiry recurs; the installation says how often,
 * as the variability system setting =inbox.approval_expiry.schedule=. This
 * reads that setting over the wire and registers the job that publishes
 * =inbox.v1.ops.expire_overdue_approvals=.
 *
 * Registering is part of starting rather than something that happens later.
 * A service that cannot register its expiry job throws, and the process
 * refuses to start rather than run with requests nobody will ever close.
 */
class approval_expiry_schedule final {
public:
    explicit approval_expiry_schedule(ores::nats::service::nats_client svc_nats);

    approval_expiry_schedule(const approval_expiry_schedule&) = delete;
    approval_expiry_schedule& operator=(const approval_expiry_schedule&) = delete;

    /**
     * @brief Registers the job, retrying while a dependency is unreachable.
     *
     * @throws application_exception when the job cannot be registered.
     */
    boost::asio::awaitable<void> register_job();

private:
    inline static std::string_view logger_name = "ores.inbox.service.app.approval_expiry_schedule";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    /**
     * @brief Why one attempt to register the job did not land.
     *
     * A retryable failure is one a later attempt can cure, which in practice
     * means a service that is not answering yet. The rest are decisions -- a
     * setting that is absent, a cron that does not parse, a scheduler that
     * refuses the write -- and repeating the attempt cannot change them, so
     * the service reports them and stops.
     */
    struct failure {
        bool retryable = false;
        std::string message;
    };

    std::expected<void, failure> try_register_once();

    ores::nats::service::nats_client svc_nats_;
};

}

#endif
