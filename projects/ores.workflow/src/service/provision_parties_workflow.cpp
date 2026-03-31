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
#include "ores.workflow/service/provision_parties_workflow.hpp"

#include <format>
#include <chrono>
#include <string_view>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.refdata.api/domain/party.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.iam.api/domain/account_party.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.workflow/domain/workflow_step.hpp"
#include "ores.workflow/repository/workflow_step_repository.hpp"

namespace ores::workflow::service {

using namespace ores::logging;

namespace {

inline static std::string_view logger_name =
    "ores.workflow.service.provision_parties_workflow";

static auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

/**
 * @brief Makes an authenticated NATS request and deserializes the response.
 *
 * Returns nullopt and logs on any error (network, X-Error header, or parse).
 * The caller should set its error_ field and return false.
 */
template<typename Req>
std::optional<typename Req::response_type>
nats_call(ores::nats::service::nats_client& nats, const Req& request,
    std::string& out_error) {
    using Resp = typename Req::response_type;
    try {
        const auto json = rfl::json::write(request);
        const auto msg = nats.authenticated_request(Req::nats_subject, json);

        const auto err_it = msg.headers.find(
            std::string(ores::nats::headers::x_error));
        if (err_it != msg.headers.end()) {
            out_error = std::format("Service error on {}: {}",
                Req::nats_subject, err_it->second);
            return std::nullopt;
        }
        const std::string_view sv(
            reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
        auto result = rfl::json::read<Resp>(sv);
        if (!result) {
            out_error = std::format("Failed to parse response from {}: {}",
                Req::nats_subject, result.error().what());
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        out_error = std::format("Exception calling {}: {}",
            Req::nats_subject, e.what());
        return std::nullopt;
    }
}

domain::workflow_step make_step(const boost::uuids::uuid& workflow_id,
    int index, std::string_view name, std::string_view request_json) {
    domain::workflow_step s;
    s.id = boost::uuids::random_generator()();
    s.workflow_id = workflow_id;
    s.step_index = index;
    s.name = std::string(name);
    s.status = "in_progress";
    s.request_json = std::string(request_json);
    s.started_at = std::chrono::system_clock::now();
    s.created_at = *s.started_at;
    return s;
}

} // namespace

provision_parties_workflow::provision_parties_workflow(
    boost::uuids::uuid workflow_id,
    messaging::provision_parties_request request,
    std::string correlation_id)
    : workflow_id_(workflow_id)
    , request_(std::move(request))
    , correlation_id_(std::move(correlation_id)) {
    party_states_.resize(request_.parties.size());
    // Pre-generate a UUID for each party so step 0 and step 2 can agree.
    boost::uuids::random_generator gen;
    for (auto& s : party_states_)
        s.party_id = gen();
}

bool provision_parties_workflow::execute(
    ores::database::context ctx,
    ores::nats::service::nats_client& nats) {
    BOOST_LOG_SEV(lg(), debug) << "Executing provision_parties_workflow for "
                               << request_.parties.size() << " parties.";

    repository::workflow_step_repository step_repo;
    const int n = static_cast<int>(request_.parties.size());

    for (int i = 0; i < n; ++i) {
        const auto& input = request_.parties[i];
        auto& state = party_states_[i];

        // ----------------------------------------------------------------
        // Step 0: save party
        // ----------------------------------------------------------------
        ores::refdata::domain::party party_obj;
        party_obj.version = 0;
        party_obj.id = state.party_id;
        party_obj.full_name = input.full_name;
        party_obj.short_code = input.short_code;
        // codename is auto-generated by the DB trigger when left empty
        party_obj.party_category = input.party_category;
        party_obj.party_type = input.party_type;
        party_obj.business_center_code = input.business_center_code;
        if (input.parent_party_id) {
            try {
                party_obj.parent_party_id =
                    boost::lexical_cast<boost::uuids::uuid>(*input.parent_party_id);
            } catch (const boost::bad_lexical_cast&) {
                error_ = std::format("Invalid parent_party_id '{}' for party {}: "
                    "not a valid UUID", *input.parent_party_id, i);
                return false;
            }
        }
        party_obj.status = "Inactive"; // wizard fires on first login
        party_obj.change_commentary = "Provisioned by workflow";
        party_obj.recorded_at = std::chrono::system_clock::now();

        ores::refdata::messaging::save_party_request save_party{
            .data = std::move(party_obj)};
        const auto party_json = rfl::json::write(save_party);

        auto step0 = make_step(workflow_id_, 3 * i + 0, "save_party", party_json);
        step_repo.create(ctx, step0);

        auto party_resp = nats_call(nats, save_party, error_);
        if (!party_resp || !party_resp->success) {
            if (error_.empty())
                error_ = std::format("save_party failed for party {}: {}",
                    i, party_resp ? party_resp->message : "(no response)");
            step_repo.update_status(ctx, step0.id, "failed", "", error_);
            return false;
        }
        step_repo.update_status(ctx, step0.id, "completed",
            rfl::json::write(*party_resp), "");
        state.completed_steps = 1;

        // ----------------------------------------------------------------
        // Step 1: save account
        // ----------------------------------------------------------------
        ores::iam::messaging::save_account_request save_account{
            .principal  = input.principal,
            .password   = input.password,
            .totp_secret = input.totp_secret,
            .email      = input.email,
            .account_type = input.account_type};
        const auto account_json = rfl::json::write(save_account);

        auto step1 = make_step(workflow_id_, 3 * i + 1, "save_account",
            account_json);
        step_repo.create(ctx, step1);

        auto account_resp = nats_call(nats, save_account, error_);
        if (!account_resp || !account_resp->success) {
            if (error_.empty())
                error_ = std::format("save_account failed for party {}: {}",
                    i, account_resp ? account_resp->message : "(no response)");
            step_repo.update_status(ctx, step1.id, "failed", "", error_);
            return false;
        }
        state.account_id = account_resp->account_id;
        step_repo.update_status(ctx, step1.id, "completed",
            rfl::json::write(*account_resp), "");
        state.completed_steps = 2;

        // ----------------------------------------------------------------
        // Step 2: link account to party
        // ----------------------------------------------------------------
        ores::iam::domain::account_party link;
        link.version = 0;
        link.account_id =
            boost::lexical_cast<boost::uuids::uuid>(state.account_id);
        link.party_id = state.party_id;
        link.change_commentary = "Provisioned by workflow";

        ores::iam::messaging::save_account_party_request save_link{
            .account_parties = {std::move(link)}};
        const auto link_json = rfl::json::write(save_link);

        auto step2 = make_step(workflow_id_, 3 * i + 2, "save_account_party",
            link_json);
        step_repo.create(ctx, step2);

        auto link_resp = nats_call(nats, save_link, error_);
        if (!link_resp || !link_resp->success) {
            if (error_.empty())
                error_ = std::format(
                    "save_account_party failed for party {}: {}",
                    i, link_resp ? link_resp->message : "(no response)");
            step_repo.update_status(ctx, step2.id, "failed", "", error_);
            return false;
        }
        step_repo.update_status(ctx, step2.id, "completed",
            rfl::json::write(*link_resp), "");
        state.completed_steps = 3;
    }

    // Populate result
    result_.success = true;
    result_.message = "All parties provisioned successfully.";
    result_.correlation_id = correlation_id_;
    for (const auto& s : party_states_) {
        result_.party_ids.push_back(boost::uuids::to_string(s.party_id));
        result_.account_ids.push_back(s.account_id);
    }

    BOOST_LOG_SEV(lg(), debug) << "provision_parties_workflow completed.";
    return true;
}

void provision_parties_workflow::compensate(
    ores::database::context ctx,
    ores::nats::service::nats_client& nats) {
    BOOST_LOG_SEV(lg(), info) << "Compensating provision_parties_workflow.";

    // Iterate in reverse so we undo the last-attempted party first.
    for (int i = static_cast<int>(party_states_.size()) - 1; i >= 0; --i) {
        const auto& state = party_states_[i];

        // Step 2 compensation: delete account-party link
        if (state.completed_steps >= 3) {
            BOOST_LOG_SEV(lg(), info) << "Compensating: delete account-party link, party "
                                      << i;
            using ap_key = ores::iam::messaging::account_party_key;
            ores::iam::messaging::delete_account_party_request del_link{
                .keys = {ap_key{.account_id = state.account_id,
                                .party_id   = boost::uuids::to_string(state.party_id)}}};
            std::string err;
            auto r = nats_call(nats, del_link, err);
            if (!r || !r->success) {
                const auto reason = (r && !r->message.empty()) ? r->message : err;
                BOOST_LOG_SEV(lg(), error)
                    << "Compensation delete_account_party failed for party "
                    << i << ": " << reason;
            }
        }

        // Step 1 compensation: delete account
        if (state.completed_steps >= 2 && !state.account_id.empty()) {
            BOOST_LOG_SEV(lg(), info) << "Compensating: delete account, party " << i;
            ores::iam::messaging::delete_account_request del_acct{
                .account_id = state.account_id};
            std::string err;
            auto r = nats_call(nats, del_acct, err);
            if (!r || !r->success) {
                const auto reason = (r && !r->message.empty()) ? r->message : err;
                BOOST_LOG_SEV(lg(), error)
                    << "Compensation delete_account failed for party " << i
                    << ": " << reason;
            }
        }

        // Step 0 compensation: delete party
        if (state.completed_steps >= 1) {
            BOOST_LOG_SEV(lg(), info) << "Compensating: delete party " << i;
            ores::refdata::messaging::delete_party_request del_party{
                .ids = {boost::uuids::to_string(state.party_id)}};
            std::string err;
            auto r = nats_call(nats, del_party, err);
            if (!r || !r->success) {
                const auto reason = (r && !r->message.empty()) ? r->message : err;
                BOOST_LOG_SEV(lg(), error)
                    << "Compensation delete_party failed for party " << i
                    << ": " << reason;
            }
        }
    }

    BOOST_LOG_SEV(lg(), info) << "provision_parties_workflow compensation complete.";
}

}
