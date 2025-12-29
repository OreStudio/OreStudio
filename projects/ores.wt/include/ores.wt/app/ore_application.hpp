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
#include <Wt/WLineEdit.h>
#include <Wt/WPushButton.h>
#include <Wt/WText.h>
#include <boost/uuid/uuid.hpp>
#include "ores.wt/app/login_widget.hpp"
#include "ores.wt/service/session_manager.hpp"

namespace ores::wt::app {

class currency_list_widget;
class account_list_widget;

/**
 * @brief Main Wt application for ORE Studio web interface.
 */
class ore_application : public Wt::WApplication {
public:
    explicit ore_application(const Wt::WEnvironment& env);

    static ore_application* instance();

    service::session_manager& session_manager() { return session_manager_; }
    bool is_logged_in() const { return session_manager_.is_logged_in(); }

private:
    void show_login();
    void show_bootstrap();
    void show_main_view();
    void on_login_attempt(const std::string& username, const std::string& password);
    void on_bootstrap_create(const std::string& username, const std::string& email,
                             const std::string& password);
    void on_logout();
    void setup_theme();

    void setup_currency_handlers();
    void load_currencies();
    void show_add_currency_dialog();
    void show_edit_currency_dialog(const std::string& iso_code);
    void confirm_delete_currency(const std::string& iso_code);

    void setup_account_handlers();
    void load_accounts();
    void show_add_account_dialog();
    void show_edit_account_dialog(const boost::uuids::uuid& id);
    void confirm_delete_account(const boost::uuids::uuid& id);
    void confirm_lock_account(const boost::uuids::uuid& id);
    void confirm_unlock_account(const boost::uuids::uuid& id);

    service::session_manager session_manager_;
    login_widget* login_widget_ = nullptr;
    currency_list_widget* currency_list_widget_ = nullptr;
    account_list_widget* account_list_widget_ = nullptr;

    // Bootstrap form widgets
    Wt::WLineEdit* bootstrap_username_ = nullptr;
    Wt::WLineEdit* bootstrap_email_ = nullptr;
    Wt::WLineEdit* bootstrap_password_ = nullptr;
    Wt::WLineEdit* bootstrap_confirm_ = nullptr;
    Wt::WPushButton* bootstrap_button_ = nullptr;
    Wt::WText* bootstrap_status_ = nullptr;
};

}

#endif
