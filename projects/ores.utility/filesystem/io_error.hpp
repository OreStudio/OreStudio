/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_UTILITY_FILESYSTEM_IO_ERROR_HPP
#define ORES_UTILITY_FILESYSTEM_IO_ERROR_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <string>
#include <boost/exception/info.hpp>

namespace ores::utility::filesystem {

/**
 * @brief An error occurred whilst performing an IO operation.
 */
class io_error : public virtual std::exception,
                 public virtual boost::exception {
public:
    io_error() = default;
    ~io_error() noexcept override = default;
    explicit io_error(std::string message) : message_(std::move(message)) { }
    const char* what() const noexcept override { return(message_.c_str()); }

private:
    std::string message_;
};

}

#endif
