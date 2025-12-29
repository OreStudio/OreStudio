/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_WT_APP_ORE_APPLICATION_HPP
#define ORES_WT_APP_ORE_APPLICATION_HPP

#include <Wt/WApplication.h>
#include <boost/uuid/uuid.hpp>
#include "ores.wt/app/login_widget.hpp"

namespace ores::wt::app {

class currency_list_widget;

/**
 * @brief Session information for logged-in user.
 */
struct session_info {
    boost::uuids::uuid account_id;
    std::string username;
    std::string email;
    bool is_logged_in = false;
};

/**
 * @brief Main Wt application for ORE Studio web interface.
 */
class ore_application : public Wt::WApplication {
public:
    explicit ore_application(const Wt::WEnvironment& env);

    static ore_application* instance();

    const session_info& session() const { return session_; }
    bool is_logged_in() const { return session_.is_logged_in; }

private:
    void show_login();
    void show_main_view();
    void on_login_success(const login_result& result);
    void on_logout();
    void setup_theme();

    void setup_currency_handlers();
    void load_sample_currencies();
    void show_add_currency_dialog();
    void show_edit_currency_dialog(const std::string& iso_code);
    void confirm_delete_currency(const std::string& iso_code);

    session_info session_;
    login_widget* login_widget_ = nullptr;
    currency_list_widget* currency_list_widget_ = nullptr;
};

}

#endif
