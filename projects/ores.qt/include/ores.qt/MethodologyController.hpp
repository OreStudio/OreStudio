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
#ifndef ORES_QT_METHODOLOGY_CONTROLLER_HPP
#define ORES_QT_METHODOLOGY_CONTROLLER_HPP

#include <QMdiArea>
#include <QMainWindow>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/EntityController.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/methodology.hpp"

namespace ores::qt {

class MethodologyMdiWindow;
class DetachableMdiSubWindow;

class MethodologyController final : public EntityController {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.methodology_controller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    MethodologyController(
        QMainWindow* mainWindow,
        QMdiArea* mdiArea,
        ClientManager* clientManager,
        const QString& username,
        QList<DetachableMdiSubWindow*>& allDetachableWindows,
        QObject* parent = nullptr);
    ~MethodologyController() override;

    void showListWindow() override;
    void closeAllWindows() override;

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& error);

private slots:
    void onShowDetails(const dq::domain::methodology& methodology);
    void onAddNewRequested();
    void onShowHistory(const boost::uuids::uuid& id);
    void onRevertVersion(const dq::domain::methodology& methodology);
    void onOpenVersion(const dq::domain::methodology& methodology, int versionNumber);

private:
    void showAddWindow();
    void showDetailWindow(const dq::domain::methodology& methodology);
    void showHistoryWindow(const boost::uuids::uuid& id);

    MethodologyMdiWindow* listWindow_;
    DetachableMdiSubWindow* listMdiSubWindow_;
    QList<DetachableMdiSubWindow*>& allDetachableWindows_;
};

}

#endif
