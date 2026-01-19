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
#include "ores.qt/MethodologyController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MethodologyMdiWindow.hpp"
#include "ores.qt/MethodologyDetailDialog.hpp"
#include "ores.qt/MethodologyHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.dq/eventing/methodology_changed_event.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    constexpr std::string_view methodology_event_name =
        eventing::domain::event_traits<
            dq::eventing::methodology_changed_event>::name;
}

MethodologyController::MethodologyController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
                       methodology_event_name, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "MethodologyController created";
}

void MethodologyController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "methodologies");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    listWindow_ = new MethodologyMdiWindow(clientManager_, username_);

    connect(listWindow_, &MethodologyMdiWindow::statusChanged,
            this, &MethodologyController::statusMessage);
    connect(listWindow_, &MethodologyMdiWindow::errorOccurred,
            this, &MethodologyController::errorMessage);
    connect(listWindow_, &MethodologyMdiWindow::showMethodologyDetails,
            this, &MethodologyController::onShowDetails);
    connect(listWindow_, &MethodologyMdiWindow::addNewRequested,
            this, &MethodologyController::onAddNewRequested);
    connect(listWindow_, &MethodologyMdiWindow::showMethodologyHistory,
            this, &MethodologyController::onShowHistory);

    const QColor iconColor(220, 220, 220);
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Methodologies");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_book_20_regular.svg", iconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<MethodologyController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Methodology list window created";
}

void MethodologyController::closeAllWindows() {
    BOOST_LOG_SEV(lg(), debug) << "closeAllWindows called";

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

void MethodologyController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void MethodologyController::onShowDetails(const dq::domain::methodology& methodology) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << methodology.id;
    showDetailWindow(methodology);
}

void MethodologyController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new methodology requested";
    showAddWindow();
}

void MethodologyController::onShowHistory(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << id;
    showHistoryWindow(id);
}

void MethodologyController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new methodology";

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new MethodologyDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog, &MethodologyDetailDialog::statusMessage,
            this, &MethodologyController::statusMessage);
    connect(detailDialog, &MethodologyDetailDialog::errorMessage,
            this, &MethodologyController::errorMessage);
    connect(detailDialog, &MethodologyDetailDialog::methodologySaved,
            this, [self = QPointer<MethodologyController>(this)](const boost::uuids::uuid& id) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Methodology saved: " << id;
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Methodology");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_book_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void MethodologyController::showDetailWindow(const dq::domain::methodology& methodology) {
    const QString identifier = QString::fromStdString(boost::uuids::to_string(methodology.id));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << methodology.id;

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new MethodologyDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setMethodology(methodology);

    connect(detailDialog, &MethodologyDetailDialog::statusMessage,
            this, &MethodologyController::statusMessage);
    connect(detailDialog, &MethodologyDetailDialog::errorMessage,
            this, &MethodologyController::errorMessage);
    connect(detailDialog, &MethodologyDetailDialog::methodologySaved,
            this, [self = QPointer<MethodologyController>(this)](const boost::uuids::uuid& id) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Methodology saved: " << id;
        self->handleEntitySaved();
    });
    connect(detailDialog, &MethodologyDetailDialog::methodologyDeleted,
            this, [self = QPointer<MethodologyController>(this), key](const boost::uuids::uuid& id) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Methodology deleted: " << id;
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Methodology: %1").arg(
        QString::fromStdString(methodology.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_book_20_regular.svg", iconColor));

    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<MethodologyController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void MethodologyController::showHistoryWindow(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for methodology: " << id;

    const QString idStr = QString::fromStdString(boost::uuids::to_string(id));
    const QString windowKey = build_window_key("history", idStr);

    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << id;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << id;
    const QColor iconColor(220, 220, 220);

    auto* historyDialog = new MethodologyHistoryDialog(id, clientManager_, mainWindow_);

    connect(historyDialog, &MethodologyHistoryDialog::statusChanged,
            this, [self = QPointer<MethodologyController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &MethodologyHistoryDialog::errorOccurred,
            this, [self = QPointer<MethodologyController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &MethodologyHistoryDialog::revertVersionRequested,
            this, &MethodologyController::onRevertVersion);
    connect(historyDialog, &MethodologyHistoryDialog::openVersionRequested,
            this, &MethodologyController::onOpenVersion);

    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Methodology History: %1").arg(idStr));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<MethodologyController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void MethodologyController::onOpenVersion(
    const dq::domain::methodology& methodology, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for methodology: " << methodology.id;

    const QString idStr = QString::fromStdString(boost::uuids::to_string(methodology.id));
    const QString windowKey = build_window_key("version", QString("%1_v%2")
        .arg(idStr).arg(versionNumber));

    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new MethodologyDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setMethodology(methodology);
    detailDialog->setReadOnly(true);

    connect(detailDialog, &MethodologyDetailDialog::statusMessage,
            this, [self = QPointer<MethodologyController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &MethodologyDetailDialog::errorMessage,
            this, [self = QPointer<MethodologyController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Methodology: %1 (Version %2)")
        .arg(QString::fromStdString(methodology.name)).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<MethodologyController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void MethodologyController::onRevertVersion(
    const dq::domain::methodology& methodology) {
    BOOST_LOG_SEV(lg(), info) << "Reverting methodology to version: "
                              << methodology.version;

    auto* detailDialog = new MethodologyDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setMethodology(methodology);
    detailDialog->setCreateMode(false);

    connect(detailDialog, &MethodologyDetailDialog::statusMessage,
            this, &MethodologyController::statusMessage);
    connect(detailDialog, &MethodologyDetailDialog::errorMessage,
            this, &MethodologyController::errorMessage);
    connect(detailDialog, &MethodologyDetailDialog::methodologySaved,
            this, [self = QPointer<MethodologyController>(this)](const boost::uuids::uuid& id) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Methodology reverted: " << id;
        emit self->statusMessage(QString("Methodology reverted successfully"));
        self->handleEntitySaved();
    });

    const QColor iconColor(220, 220, 220);
    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Methodology: %1")
        .arg(QString::fromStdString(methodology.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* MethodologyController::listWindow() const {
    return listWindow_;
}

}
