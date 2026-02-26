/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024-2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_ABOUT_DIALOG_HPP
#define ORES_QT_ABOUT_DIALOG_HPP

#include <QWidget>
#include "ui_AboutDialog.h"
#include "ores.qt/LogoLabel.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

class ClientManager;

/**
 * @brief Widget displaying application version, build metadata,
 * and runtime system information, embedded in an MDI subwindow.
 *
 * The widget has two tabs:
 * - **About**: project link and description text
 * - **System Info**: three-row tree showing database, server, and client
 *   version strings populated from the ClientManager's system_info_entries.
 */
class AboutDialog final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.about_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit AboutDialog(ClientManager* clientManager = nullptr,
                         QWidget* parent = nullptr);
    ~AboutDialog() override;

protected:
    void showEvent(QShowEvent* e) override;

private:
    /**
     * @brief Populate the System Info tree with database, server, and client rows.
     *
     * Reads KVP entries from the ClientManager (fetched at login time).
     * Shows "(not connected)" for database and server rows when no data
     * is available.
     */
    void populateSystemInfo();
    Ui::AboutDialog ui_;
    LogoLabel* logoLabel_;
    ClientManager* clientManager_;
};

}

#endif
