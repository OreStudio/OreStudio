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
#ifndef ORES_WT_APP_ACCOUNT_LIST_WIDGET_HPP
#define ORES_WT_APP_ACCOUNT_LIST_WIDGET_HPP

#include <Wt/WContainerWidget.h>
#include <Wt/WTable.h>
#include <Wt/WPushButton.h>
#include <Wt/WText.h>
#include <Wt/WSignal.h>
#include <vector>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::wt::app {

/**
 * @brief Account data for display.
 */
struct account_row {
    boost::uuids::uuid id;
    std::string username;
    std::string email;
    bool locked = false;
    bool online = false;
    int failed_logins = 0;
    int version = 0;
};

/**
 * @brief Widget displaying list of user accounts with CRUD operations.
 */
class account_list_widget : public Wt::WContainerWidget {
public:
    account_list_widget();

    Wt::Signal<>& add_requested() { return add_requested_; }
    Wt::Signal<boost::uuids::uuid>& edit_requested() { return edit_requested_; }
    Wt::Signal<boost::uuids::uuid>& delete_requested() { return delete_requested_; }
    Wt::Signal<boost::uuids::uuid>& lock_requested() { return lock_requested_; }
    Wt::Signal<boost::uuids::uuid>& unlock_requested() { return unlock_requested_; }

    void refresh();
    void set_accounts(const std::vector<account_row>& accounts);

private:
    void setup_toolbar();
    void setup_table();
    void populate_table();

    Wt::WTable* table_;
    Wt::WText* status_text_;
    Wt::WPushButton* add_button_;
    Wt::WPushButton* refresh_button_;

    std::vector<account_row> accounts_;
    Wt::Signal<> add_requested_;
    Wt::Signal<boost::uuids::uuid> edit_requested_;
    Wt::Signal<boost::uuids::uuid> delete_requested_;
    Wt::Signal<boost::uuids::uuid> lock_requested_;
    Wt::Signal<boost::uuids::uuid> unlock_requested_;
};

}

#endif
