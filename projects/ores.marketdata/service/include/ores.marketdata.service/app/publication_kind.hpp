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
#ifndef ORES_MARKETDATA_SERVICE_APP_PUBLICATION_KIND_HPP
#define ORES_MARKETDATA_SERVICE_APP_PUBLICATION_KIND_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.marketdata.service/app/derived_market_object_kind.hpp"
#include "ores.marketdata.service/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>

namespace ores::marketdata::service::app {

/**
 * @brief One subject a publish is about, in the shape every kind shares.
 *
 * The kind states what the subject is and the store says who owns it, so a
 * caller names the subject and never the party or the series: a publish cannot
 * put one party's set under another's name.
 *
 * @c subject_key is what the caller named -- a configuration's identifier, a
 * currency pair, a blueprint's identifier -- and @c subject_name is what a
 * person reads in a refusal or a notice. They differ because a caller holds an
 * identifier and a person reads a curve's URI or a pair's code.
 */
struct publication_subject {
    derived_kind kind{derived_kind::curve};
    boost::uuids::uuid party_id{};
    /// The identifier the caller named, when the kind names one with a uuid.
    boost::uuids::uuid subject_id{};
    std::string subject_key;
    std::string subject_name;
    boost::uuids::uuid output_series_id{};
    std::chrono::system_clock::time_point as_of{};
};

/**
 * @brief One published set, as every kind states it.
 *
 * The facts are what every act of the lifecycle needs to name a set: which set
 * it is, which decision published it, when and by whom, and the subject it is a
 * set of. A kind's own record holds the values and the checks beside these.
 */
struct publication_facts {
    publication_subject subject;
    boost::uuids::uuid publish_id{};
    boost::uuids::uuid request_id{};
    std::uint32_t version{};
    std::chrono::system_clock::time_point published_at{};
    std::string published_by;
    /// The as-of of the set this one replaced, when the series had one in use.
    std::optional<std::chrono::system_clock::time_point> replaced_as_of;
};

/**
 * @brief Whether a request may publish its subject, and the set it would write.
 *
 * @c approved says the publish may go ahead. @c state_code is the request's
 * state as the record holds it, so a caller reads waiting or refused rather
 * than guessing. @c message says why the publish may not go ahead, in the words
 * the person reads.
 */
struct publication_verdict {
    bool approved{false};
    std::string state_code;
    std::string message;
    std::optional<publication_facts> facts;
};

/**
 * @brief What one kind supplies to the lifecycle, and the only way it is reached.
 *
 * The acts of the lifecycle -- raise, decide, apply, supersede, notice -- are
 * written once, in publication_lifecycle.cpp, and each calls one of these. A
 * kind supplies the facts of its subject and its words, and never a copy of an
 * act: a fourth kind is one implementation of this interface and a row in the
 * declaration.
 *
 * Every method that writes takes the transaction it must write in, so the acts
 * decide what is one transaction and a kind cannot open its own.
 */
class ORES_MARKETDATA_SERVICE_EXPORT publication_kind {
public:
    virtual ~publication_kind() = default;

    virtual derived_kind kind() const = 0;

    /**
     * @brief The subject a caller names, as the store holds it.
     *
     * The party and the output series are the subject's to say, so a caller
     * cannot publish one party's set under another's name.
     *
     * @throws std::invalid_argument when the subject cannot be named, with the
     * reason in the words a person reads.
     */
    virtual publication_subject
    resolve_subject(const ores::database::context& ctx,
                    const boost::uuids::uuid& party_id,
                    const std::string& subject_key,
                    std::chrono::system_clock::time_point as_of) const = 0;

    /**
     * @brief Why this subject may not be raised, or nothing when it may.
     *
     * The checks that block a raise, a request already open for the same
     * subject at the same as-of, and a candidate older than the set in use. A
     * refusal here means no decider is asked to answer for a set the service
     * would not publish.
     */
    virtual std::optional<std::string>
    refusal_of_raise(const ores::database::context& ctx,
                     const publication_subject& subject) const = 0;

    /**
     * @brief Writes the subject part, and the candidate it names, in the raise's transaction.
     *
     * The row names the request, so the decision covers exactly these facts and
     * no others.
     */
    virtual void write_publish(const ores::database::context& tx,
                               const boost::uuids::uuid& request_id,
                               const publication_subject& subject) const = 0;

    /**
     * @brief Whether this request permits this publish, and the set it would write.
     *
     * The request must be of the kind the declaration names, it must be
     * approved, the subject it names must be the subject asked about, and it
     * must not have published already.
     */
    virtual publication_verdict verdict_on_publish(const ores::database::context& ctx,
                                                   const std::string& request_id,
                                                   const publication_subject& attempted) const = 0;

    /**
     * @brief Computes what the set needs before it is published, or says the apply must wait.
     *
     * A reason here defers the apply and writes nothing, so a request whose
     * inputs have not arrived is not spent.
     */
    virtual std::optional<std::string> prepare(const ores::database::context& ctx,
                                               const publication_subject& subject) const = 0;

    /**
     * @brief Publishes the set and ends every live set of its series, in the caller's transaction.
     *
     * @param tx The transaction the apply opened, so the published set and the
     * end of the set it replaces are one act.
     * @param set The facts as the verdict stated them, with the publisher and
     * the instant the apply read.
     * @return The facts as the store holds them, with the set it replaced.
     */
    virtual publication_facts publish(const ores::database::context& tx,
                                      publication_facts set) const = 0;

    /**
     * @brief Tells the readers that the set moved.
     *
     * Called after the apply's transaction commits, because telling is never
     * the act: a notice that fails leaves the set published.
     */
    virtual void announce_published(const ores::database::context& ctx,
                                    const publication_facts& set) const = 0;
};

/// The curve's implementation of the interface.
ORES_MARKETDATA_SERVICE_EXPORT const publication_kind& curve_publication_kind();

/// The forward set's implementation of the interface.
ORES_MARKETDATA_SERVICE_EXPORT const publication_kind& fx_forward_publication_kind();

/// The volatility surface's implementation of the interface.
ORES_MARKETDATA_SERVICE_EXPORT const publication_kind& fx_vol_surface_publication_kind();

/**
 * @brief The implementation the declaration names, for a kind.
 */
ORES_MARKETDATA_SERVICE_EXPORT const publication_kind& publication_kind_of(derived_kind kind);

}

#endif
