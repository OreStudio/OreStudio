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
#ifndef ORES_QT_CONNECTION_DETAIL_PANEL_HPP
#define ORES_QT_CONNECTION_DETAIL_PANEL_HPP

#include <QWidget>
#include <QStackedWidget>
#include <QLabel>
#include <QVBoxLayout>
#include <QFormLayout>
#include <boost/uuid/uuid.hpp>
#include "ores.connections/domain/folder.hpp"
#include "ores.connections/domain/server_environment.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::connections::service {
class connection_manager;
}

namespace ores::qt {

/**
 * @brief Detail panel showing read-only information about selected item.
 *
 * Displays contextual information based on current selection:
 * - Empty state: Welcome message with quick actions
 * - Folder: Name and item count
 * - Environment: Connection details and tags (read-only)
 */
class ConnectionDetailPanel : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.connection_detail_panel";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ConnectionDetailPanel(
        connections::service::connection_manager* manager,
        QWidget* parent = nullptr);
    ~ConnectionDetailPanel() override;

    void showEmptyState();
    void showFolder(const connections::domain::folder& folder, int itemCount);
    void showEnvironment(const connections::domain::server_environment& env);

private:
    void setupEmptyPage();
    void setupFolderPage();
    void setupEnvironmentPage();
    void updateTagBadges(const boost::uuids::uuid& envId);

    connections::service::connection_manager* manager_;

    QStackedWidget* stackedWidget_;

    // Empty state page
    QWidget* emptyPage_;

    // Folder page
    QWidget* folderPage_;
    QLabel* folderNameLabel_;
    QLabel* folderItemCountLabel_;
    QLabel* folderDescriptionLabel_;

    // Environment page
    QWidget* environmentPage_;
    QLabel* envNameLabel_;
    QLabel* envHostLabel_;
    QLabel* envPortLabel_;
    QLabel* envUsernameLabel_;
    QLabel* envDescriptionLabel_;
    QWidget* envTagsContainer_;
};

}

#endif
