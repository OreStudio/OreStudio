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
#ifndef ORES_QT_MAIN_TAB_HPP
#define ORES_QT_MAIN_TAB_HPP

#include <QTabWidget>
#include <memory>
#include "ores.comms/net/client.hpp"
#include "ores.utility/log/make_logger.hpp"

namespace ores::qt {

class MainTabWidget : public QTabWidget {
    Q_OBJECT

private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.qt.main_tab_widget");
        return instance;
    }

public:
    explicit MainTabWidget(QWidget* parent = nullptr);

    /**
     * @brief Set the client for server communication.
     */
    void setClient(std::shared_ptr<comms::client> client) {
        client_ = std::move(client);
    }

public slots:
    void openCurrencyTabPage();
    void closeTab(const int& index);

protected:
    void paintEvent(QPaintEvent* e ) override;

private:
    int currenciesIndex_;
    std::shared_ptr<comms::client> client_;
};

}

#endif
