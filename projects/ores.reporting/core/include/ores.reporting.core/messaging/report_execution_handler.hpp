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
#ifndef ORES_REPORTING_MESSAGING_REPORT_EXECUTION_HANDLER_HPP
#define ORES_REPORTING_MESSAGING_REPORT_EXECUTION_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.service/service/cache/run_token_cache.hpp"
#include "ores.workflow.core/service/fsm_state_map.hpp"
#include <optional>
#include <string>

namespace ores::reporting::messaging {

/**
 * @brief Workflow step handlers for report execution.
 *
 * Handles fire-and-forget commands dispatched by the workflow engine:
 *
 *  reporting.v1.ops.gather_trades           — trades in the report's scope
 *  reporting.v1.ops.gather_market_data      — the series those trades need
 *  reporting.v1.ops.assemble_bundle         — persists the gathered input
 *  reporting.v1.ops.resolve_prepared_input  — pre-processing, substituted
 *  reporting.v1.ops.collect_compute_results — post-processing
 *  reporting.v1.ops.ignore_compute_results  — post-processing, substituted
 *  reporting.v1.ops.finalise_report                — marks instance as completed
 *  reporting.v1.ops.fail_report                    — marks instance as failed (compensation)
 */
class ORES_REPORTING_CORE_EXPORT report_execution_handler {
private:
    inline static std::string_view logger_name =
        "ores.reporting.messaging.report_execution_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    report_execution_handler(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             ores::nats::service::nats_client svc_nats,
                             ores::workflow::service::fsm_state_map instance_states,
                             std::string http_base_url);

    void gather_trades(ores::nats::message msg);
    void gather_market_data(ores::nats::message msg);
    void assemble_bundle(ores::nats::message msg);
    void collect_results(ores::nats::message msg);
    void resolve_prepared_input(ores::nats::message msg);
    void ignore_compute_results(ores::nats::message msg);
    void finalise(ores::nats::message msg);
    void fail(ores::nats::message msg);

private:
    void mark_instance_failed(const std::string& tenant_id,
                              const std::string& instance_id,
                              const std::string& error_message);

    /**
     * @brief A client that carries the run's token, for calls to an owner.
     *
     * The token comes from the cache, keyed by the grant the report instance
     * recorded at admission and the run. @p renew drops the held token first,
     * which is how a caller that read =token_expired= exchanges once and
     * repeats its request.
     *
     * @return The client, or nothing when the instance holds no grant or the
     * exchange refuses; @p error then says why.
     */
    std::optional<ores::nats::service::nats_client> run_token_client(const std::string& tenant_id,
                                                                     const std::string& run_id,
                                                                     bool renew,
                                                                     std::string& error);

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::nats::service::nats_client svc_nats_;
    ores::service::service::cache::run_token_cache run_tokens_;
    ores::workflow::service::fsm_state_map instance_states_;
    std::string http_base_url_;
};

}

#endif
