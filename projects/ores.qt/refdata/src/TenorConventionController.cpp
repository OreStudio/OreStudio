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
#include "ores.qt/TenorConventionController.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/TenorConventionDetailDialog.hpp"
#include "ores.qt/TenorConventionHistoryDialog.hpp"
#include "ores.qt/TenorConventionMdiWindow.hpp"
#include "ores.qt/UiPersistence.hpp"
#include "ores.refdata.api/eventing/tenor_convention_changed_event.hpp"
#include <QMdiSubWindow>
#include <QMessageBox>
#include <QPointer>

namespace ores::qt {

using namespace ores::logging;

namespace {
constexpr std::string_view convention_event_name =
    eventing::domain::event_traits<refdata::eventing::tenor_convention_changed_event>::name;
}

TenorConventionController::TenorConventionController(QMainWindow* mainWindow,
                                                     QMdiArea* mdiArea,
                                                     ClientManager* clientManager,
                                                     const QString& username,
                                                     QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username, convention_event_name, parent)
    , listWindow_(nullptr)
    , listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "TenorConventionController created";
}

void TenorConventionController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "tenor_conventions");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    // Create new window
    listWindow_ = new TenorConventionMdiWindow(clientManager_, username_);

    // Connect signals
    connect(listWindow_,
            &TenorConventionMdiWindow::statusChanged,
            this,
            &TenorConventionController::statusMessage);
    connect(listWindow_,
            &TenorConventionMdiWindow::errorOccurred,
            this,
            &TenorConventionController::errorMessage);
    connect(listWindow_,
            &TenorConventionMdiWindow::showConventionDetails,
            this,
            &TenorConventionController::onShowDetails);
    connect(listWindow_,
            &TenorConventionMdiWindow::addNewRequested,
            this,
            &TenorConventionController::onAddNewRequested);
    connect(listWindow_,
            &TenorConventionMdiWindow::showConventionHistory,
            this,
            &TenorConventionController::onShowHistory);

    // Create MDI subwindow
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Tenor Conventions");
    listMdiSubWindow_->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Table, IconUtils::DefaultIconColor));
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
            [self = QPointer<TenorConventionController>(this), key]() {
                if (!self)
                    return;
                self->untrack_window(key);
                self->listWindow_ = nullptr;
                self->listMdiSubWindow_ = nullptr;
            });

    BOOST_LOG_SEV(lg(), debug) << "Tenor Convention list window created";
}

void TenorConventionController::closeAllWindows() {
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

void TenorConventionController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void TenorConventionController::onShowDetails(const refdata::domain::tenor_convention& convention) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << convention.code;
    showDetailWindow(convention);
}

void TenorConventionController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new tenor convention requested";
    showAddWindow();
}


void TenorConventionController::onShowHistory(const refdata::domain::tenor_convention& convention) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: " << convention.code;
    showHistoryWindow(QString::fromStdString(convention.code));
}

void TenorConventionController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new tenor convention";

    auto* detailDialog = new TenorConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);

    connect(detailDialog,
            &TenorConventionDetailDialog::statusMessage,
            this,
            &TenorConventionController::statusMessage);
    connect(detailDialog,
            &TenorConventionDetailDialog::errorMessage,
            this,
            &TenorConventionController::errorMessage);
    connect(detailDialog,
            &TenorConventionDetailDialog::conventionSaved,
            this,
            [self = QPointer<TenorConventionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Tenor Convention saved: " << code.toStdString();
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Tenor Convention");
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Table, IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void TenorConventionController::showDetailWindow(
    const refdata::domain::tenor_convention& convention) {

    const QString identifier = QString::fromStdString(convention.code);
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: " << convention.code;

    auto* detailDialog = new TenorConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setConvention(convention);

    connect(detailDialog,
            &TenorConventionDetailDialog::statusMessage,
            this,
            &TenorConventionController::statusMessage);
    connect(detailDialog,
            &TenorConventionDetailDialog::errorMessage,
            this,
            &TenorConventionController::errorMessage);
    connect(detailDialog,
            &TenorConventionDetailDialog::conventionSaved,
            this,
            [self = QPointer<TenorConventionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Tenor Convention saved: " << code.toStdString();
                self->handleEntitySaved();
            });
    connect(detailDialog,
            &TenorConventionDetailDialog::conventionDeleted,
            this,
            [self = QPointer<TenorConventionController>(this), key](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Tenor Convention deleted: " << code.toStdString();
                self->handleEntityDeleted();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Tenor Convention: %1").arg(identifier));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::Table, IconUtils::DefaultIconColor));

    // Track window
    track_window(key, detailWindow);
    register_detachable_window(detailWindow);
    detailWindow->setGeometryKey(key);

    QPointer<TenorConventionController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void TenorConventionController::showHistoryWindow(const QString& code) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for tenor convention: "
                              << code.toStdString();

    const QString windowKey = build_window_key("history", code);

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: " << code.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: " << code.toStdString();

    auto* historyDialog = new TenorConventionHistoryDialog(code, clientManager_, mainWindow_);

    connect(historyDialog,
            &TenorConventionHistoryDialog::statusChanged,
            this,
            [self = QPointer<TenorConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(historyDialog,
            &TenorConventionHistoryDialog::errorOccurred,
            this,
            [self = QPointer<TenorConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });
    connect(historyDialog,
            &TenorConventionHistoryDialog::revertVersionRequested,
            this,
            &TenorConventionController::onRevertVersion);
    connect(historyDialog,
            &TenorConventionHistoryDialog::openVersionRequested,
            this,
            &TenorConventionController::onOpenVersion);

    // Load history data
    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(QString("Tenor Convention History: %1").arg(code));
    historyWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));
    connect_dialog_close(historyDialog, historyWindow);

    // Track this history window
    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);
    historyWindow->setGeometryKey(windowKey);

    QPointer<TenorConventionController> self = this;
    connect(historyWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void TenorConventionController::onOpenVersion(const refdata::domain::tenor_convention& convention,
                                              int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for tenor convention: " << convention.code;

    const QString code = QString::fromStdString(convention.code);
    const QString windowKey =
        build_window_key("version", QString("%1_v%2").arg(code).arg(versionNumber));

    // Try to reuse existing window
    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    auto* detailDialog = new TenorConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setConvention(convention);
    detailDialog->setReadOnly(true);

    connect(detailDialog,
            &TenorConventionDetailDialog::statusMessage,
            this,
            [self = QPointer<TenorConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->statusMessage(message);
            });
    connect(detailDialog,
            &TenorConventionDetailDialog::errorMessage,
            this,
            [self = QPointer<TenorConventionController>(this)](const QString& message) {
                if (!self)
                    return;
                emit self->errorMessage(message);
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Tenor Convention: %1 (Version %2)").arg(code).arg(versionNumber));
    detailWindow->setWindowIcon(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<TenorConventionController> self = this;
    connect(detailWindow, &QObject::destroyed, this, [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void TenorConventionController::onRevertVersion(
    const refdata::domain::tenor_convention& convention) {
    BOOST_LOG_SEV(lg(), info) << "Reverting tenor convention to version: " << convention.version;

    // Open detail dialog with the old version data for editing
    auto* detailDialog = new TenorConventionDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    auto reverted_convention = convention;
    reverted_convention.version = 0;
    detailDialog->setConvention(reverted_convention);
    detailDialog->setCreateMode(false);
    detailDialog->markDirty();

    connect(detailDialog,
            &TenorConventionDetailDialog::statusMessage,
            this,
            &TenorConventionController::statusMessage);
    connect(detailDialog,
            &TenorConventionDetailDialog::errorMessage,
            this,
            &TenorConventionController::errorMessage);
    connect(detailDialog,
            &TenorConventionDetailDialog::conventionSaved,
            this,
            [self = QPointer<TenorConventionController>(this)](const QString& code) {
                if (!self)
                    return;
                BOOST_LOG_SEV(lg(), info) << "Tenor Convention reverted: " << code.toStdString();
                emit self->statusMessage(
                    QString("Tenor Convention '%1' reverted successfully").arg(code));
                self->handleEntitySaved();
            });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(
        QString("Revert Tenor Convention: %1").arg(QString::fromStdString(convention.code)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                               IconUtils::DefaultIconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

EntityListMdiWindow* TenorConventionController::listWindow() const {
    return listWindow_;
}

void TenorConventionController::notifyOpenDialogs(const QStringList& entityIds) {
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
