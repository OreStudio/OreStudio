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
#ifndef ORES_REPORTING_CORE_SERVICE_CUT_BODY_READER_HPP
#define ORES_REPORTING_CORE_SERVICE_CUT_BODY_READER_HPP

#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/service/cut_assembly.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>

namespace ores::reporting::service {

/**
 * @brief The scope and the instant an assembly reads the store at.
 *
 * The party is the session's: a cut fixes the books of the party that took it,
 * so the reads a person's take makes are the reads their session may make.
 */
struct cut_body_query {
    boost::uuids::uuid party_id;

    /** @brief The rates centre whose market data the scope revalues against. */
    std::string business_centre_code;

    /** @brief The instant the body is fixed at. */
    std::chrono::system_clock::time_point timepoint;
};

/**
 * @brief What the store answers about the body of one would-be cut.
 *
 * The assembly states which part is closed from these facts and which gap
 * stands, and it reads no store of its own. The interface is what makes that
 * rule testable without a database and what keeps the components' own reads
 * where they belong: the population is trading's to answer and the sets are
 * market data's, so this is the one seam between the assembly and them.
 */
class ORES_REPORTING_CORE_EXPORT cut_body_reader {
public:
    virtual ~cut_body_reader() = default;

    /**
     * @brief Answers every part of the body the store can answer.
     *
     * A part the store cannot answer is absent, with the reason the read gave,
     * and never an exception: the assembly records it as a gap.
     */
    [[nodiscard]] virtual cut_body_facts read(const cut_body_query& query) = 0;
};

}

#endif
