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
#include "ores.qt/YieldCurveProcessParameterDefinitionController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.qt/YieldCurveProcessParameterDefinitionMdiWindow.hpp"
#include "ores.synthetic.api/eventing/yield_curve_process_parameter_definition_changed_event.hpp"
#include "ores.synthetic.api/messaging/yield_curve_process_parameter_definition_protocol.hpp"
#include <QFutureWatcher>
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view parameter_definition_event_name = eventing::domain::event_traits<
    synthetic::eventing::yield_curve_process_parameter_definition_changed_event>::name;
}

YieldCurveProcessParameterDefinitionController::YieldCurveProcessParameterDefinitionController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(
          mainWindow, mdiArea, clientManager, username, parameter_definition_event_name, parent)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "YieldCurveProcessParameterDefinitionController created";
}

void YieldCurveProcessParameterDefinitionController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "yield_curve_process_parameter_definitions");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new YieldCurveProcessParameterDefinitionMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &YieldCurveProcessParameterDefinitionMdiWindow::statusChanged,
            this,
            &YieldCurveProcessParameterDefinitionController::statusMessage);
    connect(listWindow_,
            &YieldCurveProcessParameterDefinitionMdiWindow::errorOccurred,
            this,
            &YieldCurveProcessParameterDefinitionController::errorMessage);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Yield Curve Process Parameter Definitions");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Tag, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    // Track window
    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);
    listMdiSubWindow_->setGeometryKey(key);
    UiPersistence::restoreMdiGeometry(key, listMdiSubWindow_);

    // Cleanup when closed
    connect(listMdiSubWindow_,
            &QObject::destroyed,
            this,
            [self = QPointer<YieldCurveProcessParameterDefinitionController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Yield Curve Process Parameter Definition list window created";
}

void YieldCurveProcessParameterDefinitionController::closeAllWindows() {
    BOOST_LOG_SEV(lg(), debug) << "closeAllWindows called";

    // Close all managed windows
    QList<QString> keys = managed_windows_.keys();
    for (const QString& key : keys) {
        if (auto* window = managed_windows_.value(key)) {
            window->close();
        }
    }
    managed_windows_.clear();

    listWindow_ = nullptr;
    listMdiSubWindow_ = nullptr;
}

void YieldCurveProcessParameterDefinitionController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}


EntityListMdiWindow* YieldCurveProcessParameterDefinitionController::listWindow() const {
    return listWindow_;
}

void YieldCurveProcessParameterDefinitionController::notifyOpenDialogs(
    const QStringList& entityIds) {
    for (auto it = managed_windows_.begin(); it != managed_windows_.end(); ++it) {
        auto* window = it.value();
        if (!window)
            continue;

        if (it.key().startsWith("details.")) {
            if (auto* dialog = qobject_cast<DetailDialogBase*>(window->widget())) {
                if (entityIds.isEmpty() || entityIds.contains(dialog->code())) {
                    dialog->markAsStale();
                }
            }
        } else if (it.key().startsWith("history.")) {
            if (auto* dialog = qobject_cast<HistoryDialogBase*>(window->widget())) {
                if (entityIds.isEmpty() || entityIds.contains(dialog->code())) {
                    dialog->markAsStale();
                }
            }
        }
    }
}

}
