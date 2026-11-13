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
#include "ores.compute.core/messaging/run_files_handler.hpp"
#include "ores.compute.api/messaging/run_files_protocol.hpp"
#include "ores.compute.core/repository/batch_repository.hpp"
#include "ores.compute.core/repository/result_repository.hpp"
#include "ores.compute.core/repository/workunit_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <map>
#include <string>

namespace ores::compute::messaging {

using namespace ores::logging;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::reply;
using ores::service::service::make_request_context;

namespace {

/**
 * @brief The most workunits a run's file list reads.
 *
 * A report run dispatches a handful of parts. The limit is the largest page
 * the listing calls answer with, so a run that exceeds it is a defect and not
 * a page to follow.
 */
constexpr std::uint32_t k_workunit_limit = 1000;

/// The newest of the batches that name this run, or nothing when none does.
std::optional<domain::batch> batch_of_run(ores::database::context ctx, const std::string& run_id) {
    repository::batch_repository batches;
    const auto found = batches.read_latest_by_external_ref(ctx, run_id);
    if (found.empty())
        return std::nullopt;
    return found.front();
}

}

void run_files_handler::list_run_files(ores::nats::message msg) {
    BOOST_LOG_SEV(lg(), debug) << "Handling " << msg.subject;

    const auto req = decode<list_run_files_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }

    auto ctx_expected = make_request_context(ctx_, msg, verifier_);
    if (!ctx_expected) {
        error_reply(nats_, msg, ctx_expected.error());
        return;
    }
    const auto& ctx = *ctx_expected;
    if (!has_permission(ctx, "compute::batches:read")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }

    list_run_files_response resp;
    resp.run_id = req->run_id;
    try {
        const auto batch = batch_of_run(ctx, req->run_id);
        if (!batch) {
            /*
             * A run whose batch is not dispatched yet states so, and the screen
             * draws an empty tree rather than an error: the run exists and has
             * written nothing.
             */
            resp.success = true;
            resp.message = "The run has no batch yet.";
            reply(nats_, msg, resp);
            return;
        }
        resp.batch_id = boost::uuids::to_string(batch->id);

        repository::workunit_repository workunits;
        const auto units = workunits.read_latest_by_batch_id(ctx,
                                                          boost::uuids::to_string(batch->id),
                                                          0,
                                                          k_workunit_limit);

        std::vector<std::string> result_ids;
        result_ids.reserve(units.size());
        for (const auto& unit : units) {
            if (!unit.canonical_result_id.is_nil())
                result_ids.push_back(boost::uuids::to_string(unit.canonical_result_id));
        }

        /*
         * The accepted results travel as one read, so a run of fifty workunits
         * costs one query and not fifty.
         */
        std::map<std::string, domain::result> result_of;
        if (!result_ids.empty()) {
            repository::result_repository results;
            for (auto& result : results.read_latest(ctx, result_ids))
                result_of.emplace(boost::uuids::to_string(result.id), std::move(result));
        }

        for (const auto& unit : units) {
            const auto unit_id = boost::uuids::to_string(unit.id);
            if (!unit.input_uri.empty())
                resp.archives.push_back(
                    run_file_archive{"input", unit_id, std::string(), unit.input_uri});

            if (unit.canonical_result_id.is_nil())
                continue;
            const auto found = result_of.find(boost::uuids::to_string(unit.canonical_result_id));
            if (found == result_of.end() || found->second.output_uri.empty())
                continue;
            resp.archives.push_back(run_file_archive{
                "output",
                unit_id,
                boost::uuids::to_string(found->second.id),
                found->second.output_uri});
        }

        resp.success = true;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "list_run_files failed: " << e.what();
        resp.success = false;
        resp.message = e.what();
    }

    reply(nats_, msg, resp);
}

}
