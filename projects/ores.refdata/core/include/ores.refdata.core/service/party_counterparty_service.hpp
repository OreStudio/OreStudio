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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_SERVICE_PARTY_COUNTERPARTY_SERVICE_HPP
#define ORES_REFDATA_SERVICE_PARTY_COUNTERPARTY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_counterparty.hpp"
#include "ores.refdata.api/messaging/party_counterparty_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/party_counterparty_repository.hpp"
#include "ores.service/messaging/approval_gate.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing party counterparties.
 *
 * Provides a higher-level interface for party counterparty operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT party_counterparty_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.party_counterparty_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a party_counterparty_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit party_counterparty_service(context ctx);

    /**
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    ::ores::refdata::messaging::list_party_counterparties_response list_party_counterparties(
        const ::ores::refdata::messaging::list_party_counterparties_request& request);
    ::ores::refdata::messaging::get_party_counterparty_response get_party_counterparty(
        const ::ores::refdata::messaging::get_party_counterparty_request& request);
    ::ores::refdata::messaging::get_many_party_counterparties_response
    get_many_party_counterparties(
        const ::ores::refdata::messaging::get_many_party_counterparties_request& request);
    ::ores::refdata::messaging::put_party_counterparty_response put_party_counterparty(
        const ::ores::refdata::messaging::put_party_counterparty_request& request);
    ::ores::refdata::messaging::put_many_party_counterparties_response
    put_many_party_counterparties(
        const ::ores::refdata::messaging::put_many_party_counterparties_request& request);
    ::ores::refdata::messaging::delete_party_counterparty_response delete_party_counterparty(
        const ::ores::refdata::messaging::delete_party_counterparty_request& request);
    ::ores::refdata::messaging::delete_many_party_counterparties_response
    delete_many_party_counterparties(
        const ::ores::refdata::messaging::delete_many_party_counterparties_request& request);
    ::ores::refdata::messaging::list_by_party_id_party_counterparties_response
    list_by_party_id_party_counterparties(
        const ::ores::refdata::messaging::list_by_party_id_party_counterparties_request& request);
    ::ores::refdata::messaging::list_by_counterparty_id_party_counterparties_response
    list_by_counterparty_id_party_counterparties(
        const ::ores::refdata::messaging::list_by_counterparty_id_party_counterparties_request&
            request);
    /**@}*/

    /**
     * @brief Lets this service's writes carry out an approved request.
     *
     * The approval policy gates the writes of this junction. A write passes
     * only when it carries out a line that an approved request holds, so the
     * applier names the request here before it writes. The pass is set in
     * process: no field of a request a client sends can set it.
     */
    void carry_out_request(std::string request_id);

    /**
     * @brief Marks this service's writes as a rehearsal.
     *
     * A preview runs the real writes in a transaction it never commits. The
     * gate lets a rehearsal through, because nothing it writes lasts.
     */
    void rehearse();

private:
    context ctx_;
    repository::party_counterparty_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const ::ores::refdata::messaging::party_counterparty_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::party_counterparty& out);

    ores::service::messaging::write_pass pass_;

    /**
     * @brief Asks the approval policy whether a put may be written.
     *
     * A link has no columns of its own, so the policy is asked with none and a
     * row that names no column covers it.
     *
     * @return nullopt when the write may go ahead, otherwise the refusal.
     */
    std::optional<ores::utility::domain::result>
    gate_put(const domain::party_counterparty& next,
             const ores::utility::domain::precondition& claim);

    /**
     * @brief Asks the approval policy whether a removal may be written.
     *
     * @return nullopt when the removal may go ahead, otherwise the refusal.
     */
    std::optional<ores::utility::domain::result>
    gate_delete(const std::vector<domain::party_counterparty>& live,
                std::optional<std::uint32_t> expected);

    /**
     * @brief True when the request named by the pass holds an unapplied line
     * for exactly this operation, link and version.
     */
    bool holds_line(const std::string& operation,
                    const boost::uuids::uuid& party_id,
                    const boost::uuids::uuid& counterparty_id,
                    int base_version);
};

}

#endif
