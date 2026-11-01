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
#ifndef ORES_REPORTING_CORE_SERVICE_NATS_CUT_BODY_READER_HPP
#define ORES_REPORTING_CORE_SERVICE_NATS_CUT_BODY_READER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/service/cut_body_reader.hpp"
#include <string>

namespace ores::reporting::service {

/**
 * @brief Reads the parts of a body through the operations that answer them.
 *
 * Each part belongs to the component that holds it, so the reader calls that
 * component's read and never its table: the deal population is trading's
 * `trading.v1.trades.list` read at the instant, the sets in use are market
 * data's `marketdata.v1.ops.list_published_curves`, and the engine is
 * compute's `compute.v1.app_versions.list`. The versions of the reporting
 * configuration in force are this component's own, so they are read here.
 *
 * The caller's own Authorization header is forwarded, so every read answers
 * with what the person's session may see rather than with what the service
 * can.
 *
 * The code revision has no read: nothing in the store records the build of
 * the services that assembled the body, so the part states that as its note.
 */
class ORES_REPORTING_CORE_EXPORT nats_cut_body_reader final : public cut_body_reader {
private:
    inline static std::string_view logger_name = "ores.reporting.service.nats_cut_body_reader";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @param ctx The caller's context, which names the tenant and the party
     * the reporting component's own reads are made in.
     * @param nats The service's client.
     * @param authorization The caller's Authorization header, forwarded.
     */
    nats_cut_body_reader(ores::database::context ctx,
                         ores::nats::service::client& nats,
                         std::string authorization);

    [[nodiscard]] cut_body_facts read(const cut_body_query& query) override;

private:
    ores::database::context ctx_;
    ores::nats::service::client& nats_;
    std::string authorization_;
};

}

#endif
