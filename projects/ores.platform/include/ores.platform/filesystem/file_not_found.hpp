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
#ifndef ORES_PLATFORM_FILESYSTEM_FILE_NOT_FOUND_HPP
#define ORES_PLATFORM_FILESYSTEM_FILE_NOT_FOUND_HPP

#include <string>
#include <boost/exception/info.hpp>

namespace ores::platform::filesystem {

/**
 * @brief File was not found.
 */
class file_not_found : public virtual std::exception,
                       public virtual boost::exception {
public:
    file_not_found() = default;
    ~file_not_found() noexcept override = default;
    explicit file_not_found(std::string message) : message_(std::move(message)) { }
    const char* what() const noexcept override { return(message_.c_str()); }

private:
    std::string message_;
};

}

#endif
