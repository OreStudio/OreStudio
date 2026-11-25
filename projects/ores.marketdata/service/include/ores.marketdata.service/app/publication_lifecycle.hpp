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
#ifndef ORES_MARKETDATA_SERVICE_APP_PUBLICATION_LIFECYCLE_HPP
#define ORES_MARKETDATA_SERVICE_APP_PUBLICATION_LIFECYCLE_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.marketdata.service/app/publication_kind.hpp"
#include "ores.marketdata.service/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <utility>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief One subject a raise asks a decider to authorise.
 *
 * The caller names the subject and the as-of and nothing else: the party, the
 * output series and the checks are the store's and the kind's to say.
 */
struct publication_request {
    std::string subject_key;
    std::chrono::system_clock::time_point as_of{};
};

/**
 * @brief What a raise did: the request, the members it carries and the ones it left out.
 *
 * @c refused names each subject the request could not carry and the reason, so
 * a caller reports the subject it left out rather than failing the set for it.
 * @c raised names the subjects the request authorises, in the order asked for.
 */
struct publication_raise {
    std::string request_id;
    std::string state_code;
    std::vector<publication_subject> raised;
    std::vector<std::pair<std::string, std::string>> refused;
};

/**
 * @brief How an apply ended, in the words every kind's answer uses.
 *
 * Published moved the set. Refused did not move it and states why. Deferred did
 * not run because an input the set declares is not there yet, so the next
 * attempt tries again.
 */
enum class publication_status { published, refused, deferred };

/**
 * @brief What an apply did.
 */
struct publication_result {
    publication_status status{publication_status::refused};
    std::string state_code;
    std::string message;
    publication_facts facts;
};

/**
 * @brief What a raise does with a member of its set that it cannot carry.
 *
 * A set that must be raised whole refuses the whole raise and names the member
 * it could not carry, so no request is left authorising a subset of what the
 * caller asked for. A set that keeps what judges acceptable raises the rest and
 * names the members it left out, which is what a party's whole set wants: one
 * refusal must not cost the party the surfaces it can publish.
 */
enum class set_raise { whole_set, keep_acceptable };

/**
 * @brief The subject a caller names, read through the kind the declaration names.
 *
 * @throws std::invalid_argument when the subject cannot be named.
 */
ORES_MARKETDATA_SERVICE_EXPORT publication_subject
resolve_publication_subject(const ores::database::context& ctx,
                            derived_kind kind,
                            const std::string& subject_key,
                            std::chrono::system_clock::time_point as_of);

/**
 * @brief Raises one request for a set of subjects, and writes every subject it carries.
 *
 * The members are judged before anything is raised, so a decider never answers
 * for a set the service would refuse, and a refusal leaves the rest of the set
 * to be raised. The request and the subject parts are one transaction, so no
 * request ever names half a set.
 *
 * Whether the raise closes without a person is never this act's to say: it is
 * the kind's policy and the deployment's, read by the inbox before the
 * transaction opens, so an auto-approved act is visible where the requests and
 * the kinds are read.
 *
 * @throws std::invalid_argument when no subject is named, the reason is empty,
 * a subject is named twice, or no member of the set can be raised.
 *
 * @param party_id The party the raise acts for. Empty takes the session's, so a
 * caller that already read the party does not state it twice.
 */
ORES_MARKETDATA_SERVICE_EXPORT publication_raise
raise_publication(const ores::database::context& ctx,
                  derived_kind kind,
                  const std::vector<publication_request>& subjects,
                  const std::string& reason,
                  const boost::uuids::uuid& party_id = {},
                  set_raise policy = set_raise::keep_acceptable);

/**
 * @brief Applies an approved request to one subject: publishes, supersedes and tells.
 *
 * The verdict is asked before anything moves, so a request that does not permit
 * the publish leaves the set as it was. The published set and the end of every
 * live set of its series are one transaction, so a series is never left with
 * two live sets or with none, and the notice is raised after that transaction
 * commits because telling is never the act.
 *
 * @param publisher The account that applied the act. Empty takes the session's
 * acting user, or the service account when the session names none.
 */
ORES_MARKETDATA_SERVICE_EXPORT publication_result
apply_publication(const ores::database::context& ctx,
                  derived_kind kind,
                  const std::string& request_id,
                  const publication_subject& attempted,
                  const std::string& publisher = {});

}

#endif
