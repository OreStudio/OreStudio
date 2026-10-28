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
#ifndef ORES_REPORTING_CORE_MESSAGING_RUN_RESULT_ROWS_HANDLER_HPP
#define ORES_REPORTING_CORE_MESSAGING_RUN_RESULT_ROWS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.reporting.api/export.hpp"
#include "ores.reporting.api/messaging/report_result_row_protocol.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <optional>

namespace ores::reporting::messaging {

/**
 * @brief The rows one run produced, as the screens read them.
 *
 * The rows are a time series rather than records a person edits: they are
 * written once, by the post-processor, and read whole by the run they belong to.
 * The read is therefore hand-written against this protocol, so a screen asks for
 * one run and the store answers from its own index rather than a page of the
 * table.
 */
class ORES_REPORTING_API_EXPORT run_result_rows_handler {
private:
    inline static std::string_view logger_name = "ores.reporting.messaging.run_result_rows_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    run_result_rows_handler(ores::nats::service::client& nats,
                            ores::database::context ctx,
                            std::optional<ores::security::jwt::jwt_authenticator> verifier);

    /**
     * @brief Answers the rows of one run, or every row the caller may read.
     *
     * A request that names a run answers with that run's rows; one that names
     * none answers with an empty page rather than a scan of the table, because a
     * screen that states no run is a screen with nothing to draw.
     */
    void list_report_result_rows(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
