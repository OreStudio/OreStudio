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
#ifndef ORES_WORKFLOW_SERVICE_PROVISION_PARTIES_DEFINITIONS_HPP
#define ORES_WORKFLOW_SERVICE_PROVISION_PARTIES_DEFINITIONS_HPP

#include <chrono>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/domain/party.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.iam.api/domain/account_party.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.workflow/messaging/workflow_protocol.hpp"
#include "ores.workflow/service/workflow_definition.hpp"
#include "ores.workflow/service/workflow_registry.hpp"

namespace ores::workflow::service {

/**
 * @brief Registers the provision_parties workflow definition.
 *
 * The provision_parties workflow has three steps per party:
 *   0. refdata.v1.parties.save      — create party record
 *   1. iam.v1.accounts.save         — create IAM account
 *   2. iam.v1.account-parties.save  — link account to party
 *
 * Compensation (reverse order):
 *   iam.v1.account-parties.delete
 *   iam.v1.accounts.delete
 *   refdata.v1.parties.delete
 *
 * Each workflow instance carries one provision_party_workflow_request as
 * its request_json.  The build_command functions deserialise that struct
 * and construct the correct per-step command payload.
 */
inline void register_provision_parties_workflow(workflow_registry& registry) {
    workflow_definition def;
    def.type_name = "provision_parties_workflow";

    // ----------------------------------------------------------------
    // Step 0: save party (refdata)
    // ----------------------------------------------------------------
    {
        workflow_step_def s;
        s.name = "save_party";
        s.command_subject = "refdata.v1.parties.save";
        s.compensation_subject = "refdata.v1.parties.delete";

        s.build_command = [](const std::string& request_json,
            const std::vector<std::string>&) -> std::string {

            auto wr = rfl::json::read<
                messaging::provision_party_workflow_request>(request_json);
            if (!wr) return "{}";
            const auto& r = *wr;

            ores::refdata::domain::party p;
            p.id = boost::lexical_cast<boost::uuids::uuid>(r.party_id);
            p.full_name = r.full_name;
            p.short_code = r.short_code;
            p.party_category = r.party_category;
            p.party_type = r.party_type;
            p.business_center_code = r.business_center_code;
            if (r.parent_party_id)
                p.parent_party_id = boost::lexical_cast<boost::uuids::uuid>(
                    *r.parent_party_id);
            p.status = r.status;
            p.change_commentary = "Provisioned by workflow";
            p.recorded_at = std::chrono::system_clock::now();

            return rfl::json::write(
                ores::refdata::messaging::save_party_request{.data = std::move(p)});
        };

        // Compensation: build delete_party_request from the original command.
        // The command_json is a save_party_request; we parse party.id from it.
        s.build_compensation = [](const std::string& cmd_json,
            const std::string&) -> std::string {

            auto cr = rfl::json::read<
                ores::refdata::messaging::save_party_request>(cmd_json);
            if (!cr) return "{}";
            return rfl::json::write(
                ores::refdata::messaging::delete_party_request{
                    .ids = {boost::uuids::to_string(cr->data.id)}});
        };

        def.steps.push_back(std::move(s));
    }

    // ----------------------------------------------------------------
    // Step 1: save account (IAM)
    // ----------------------------------------------------------------
    {
        workflow_step_def s;
        s.name = "save_account";
        s.command_subject = "iam.v1.accounts.save";
        s.compensation_subject = "iam.v1.accounts.delete";

        s.build_command = [](const std::string& request_json,
            const std::vector<std::string>&) -> std::string {

            auto wr = rfl::json::read<
                messaging::provision_party_workflow_request>(request_json);
            if (!wr) return "{}";
            const auto& r = *wr;

            ores::iam::messaging::save_account_request req{
                .principal   = r.principal,
                .password    = r.password,
                .totp_secret = r.totp_secret,
                .email       = r.email,
                .account_type = r.account_type};
            return rfl::json::write(req);
        };

        // Compensation: build delete_account_request from the step result.
        // prev[1] (save_account_response) carries account_id.
        s.build_compensation = [](const std::string&,
            const std::string& result_json) -> std::string {

            auto ar = rfl::json::read<
                ores::iam::messaging::save_account_response>(result_json);
            if (!ar || ar->account_id.empty()) return "{}";
            return rfl::json::write(
                ores::iam::messaging::delete_account_request{
                    .account_id = ar->account_id});
        };

        def.steps.push_back(std::move(s));
    }

    // ----------------------------------------------------------------
    // Step 2: link account to party (IAM)
    // ----------------------------------------------------------------
    {
        workflow_step_def s;
        s.name = "link_account_party";
        s.command_subject = "iam.v1.account-parties.save";
        s.compensation_subject = "iam.v1.account-parties.delete";

        // previous_results[0] = save_party_response   (step 0)
        // previous_results[1] = save_account_response (step 1) → account_id
        s.build_command = [](const std::string& request_json,
            const std::vector<std::string>& prev) -> std::string {

            if (prev.size() < 2) return "{}";

            auto wr = rfl::json::read<
                messaging::provision_party_workflow_request>(request_json);
            if (!wr) return "{}";

            auto ar = rfl::json::read<
                ores::iam::messaging::save_account_response>(prev[1]);
            if (!ar || ar->account_id.empty()) return "{}";

            ores::iam::domain::account_party link;
            link.account_id = boost::lexical_cast<boost::uuids::uuid>(
                ar->account_id);
            link.party_id = boost::lexical_cast<boost::uuids::uuid>(
                wr->party_id);
            link.change_commentary = "Provisioned by workflow";

            return rfl::json::write(
                ores::iam::messaging::save_account_party_request{
                    .account_parties = {std::move(link)}});
        };

        // Compensation: build delete_account_party_request from command_json.
        s.build_compensation = [](const std::string& cmd_json,
            const std::string&) -> std::string {

            auto cr = rfl::json::read<
                ores::iam::messaging::save_account_party_request>(cmd_json);
            if (!cr || cr->account_parties.empty()) return "{}";
            const auto& ap = cr->account_parties.front();

            using key_t = ores::iam::messaging::account_party_key;
            return rfl::json::write(
                ores::iam::messaging::delete_account_party_request{
                    .keys = {key_t{
                        .account_id = boost::uuids::to_string(ap.account_id),
                        .party_id   = boost::uuids::to_string(ap.party_id)}}});
        };

        def.steps.push_back(std::move(s));
    }

    registry.register_definition(std::move(def));
}

}

#endif
