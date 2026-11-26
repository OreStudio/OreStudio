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
#ifndef ORES_REFDATA_SERVICE_SERVICE_PRODUCT_APPROVAL_HPP
#define ORES_REFDATA_SERVICE_SERVICE_PRODUCT_APPROVAL_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.refdata.api/messaging/product_approval_operations_protocol.hpp"
#include "ores.refdata.service/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <span>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief One row of the resolution's answer: a book, whether it permits every
 * type, and one type it permits when it does not.
 *
 * A book that permits every type and a book that permits none both answer one
 * row naming no type, so the flag is what tells an open book from a shut one.
 */
struct product_approval_row final {
    std::string book_id;
    bool permits_every_type = false;
    std::string product_type_code;
};

/**
 * @brief Folds the resolution's rows into one answer per book.
 *
 * The rows arrive in the order of the books asked for, so a book's answer opens
 * on its first row and each later row that names it adds a type. A book whose
 * row states it permits every type takes no set at all, which is what a caller
 * reads as unconstrained.
 *
 * This is a plain function over the rows so the reading is a thing a test can
 * state without a database: an open book and a shut one are one flag apart, and
 * a mistake between them permits every product in the catalogue.
 */
ORES_REFDATA_SERVICE_EXPORT std::vector<ores::refdata::messaging::book_product_approval>
book_product_approvals_from(std::span<const product_approval_row> rows);

/**
 * @brief Reads the product types each book permits, resolved through the book,
 * its desk and its party.
 *
 * The resolution is the database's own, through
 * =ores_refdata_book_product_approvals_fn=, because the chain is one rule and a
 * second statement of it in C++ would drift from the one the write path
 * enforces. A book the caller cannot see resolves to no types, so an unreadable
 * book is answered as a shut one and not as an open one.
 *
 * @param book_ids the books to resolve, in the order the answer states them
 */
ORES_REFDATA_SERVICE_EXPORT std::vector<ores::refdata::messaging::book_product_approval>
book_product_approvals(const ores::database::context& ctx,
                       const std::vector<boost::uuids::uuid>& book_ids);

}

#endif
