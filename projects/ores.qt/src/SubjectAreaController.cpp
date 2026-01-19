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
#include "ores.qt/SubjectAreaController.hpp"

#include <QMdiSubWindow>
#include <QMessageBox>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/SubjectAreaMdiWindow.hpp"
#include "ores.qt/SubjectAreaDetailDialog.hpp"
#include "ores.qt/SubjectAreaHistoryDialog.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.dq/eventing/subject_area_changed_event.hpp"

namespace ores::qt {

using namespace ores::logging;

namespace {
    constexpr std::string_view subject_area_event_name =
        eventing::domain::event_traits<
            dq::eventing::subject_area_changed_event>::name;
}

SubjectAreaController::SubjectAreaController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
                       subject_area_event_name, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "SubjectAreaController created";
}

EntityListMdiWindow* SubjectAreaController::listWindow() const {
    return listWindow_;
}

void SubjectAreaController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "subject_areas");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing list window";
        return;
    }

    listWindow_ = new SubjectAreaMdiWindow(clientManager_, username_);

    connect(listWindow_, &SubjectAreaMdiWindow::statusChanged,
            this, &SubjectAreaController::statusMessage);
    connect(listWindow_, &SubjectAreaMdiWindow::errorOccurred,
            this, &SubjectAreaController::errorMessage);
    connect(listWindow_, &SubjectAreaMdiWindow::showSubjectAreaDetails,
            this, &SubjectAreaController::onShowDetails);
    connect(listWindow_, &SubjectAreaMdiWindow::addNewRequested,
            this, &SubjectAreaController::onAddNewRequested);
    connect(listWindow_, &SubjectAreaMdiWindow::showSubjectAreaHistory,
            this, &SubjectAreaController::onShowHistory);

    const QColor iconColor(220, 220, 220);
    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle("Subject Areas");
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_table_20_regular.svg", iconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    connect(listMdiSubWindow_, &QObject::destroyed, this, [self = QPointer<SubjectAreaController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Subject area list window created";
}

void SubjectAreaController::closeAllWindows() {
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

void SubjectAreaController::reloadListWindow() {
    if (listWindow_) {
        listWindow_->reload();
    }
}

void SubjectAreaController::onShowDetails(
    const dq::domain::subject_area& subject_area) {
    BOOST_LOG_SEV(lg(), debug) << "Show details for: " << subject_area.name
                               << " in domain: " << subject_area.domain_name;
    showDetailWindow(subject_area);
}

void SubjectAreaController::onAddNewRequested() {
    BOOST_LOG_SEV(lg(), info) << "Add new subject area requested";
    showAddWindow();
}

void SubjectAreaController::onShowHistory(const QString& name,
                                          const QString& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Show history requested for: "
                               << name.toStdString()
                               << " in domain: " << domain_name.toStdString();
    showHistoryWindow(name, domain_name);
}

void SubjectAreaController::showAddWindow() {
    BOOST_LOG_SEV(lg(), debug) << "Creating add window for new subject area";

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new SubjectAreaDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(true);
    detailDialog->loadDomains();

    connect(detailDialog, &SubjectAreaDetailDialog::statusMessage,
            this, &SubjectAreaController::statusMessage);
    connect(detailDialog, &SubjectAreaDetailDialog::errorMessage,
            this, &SubjectAreaController::errorMessage);
    connect(detailDialog, &SubjectAreaDetailDialog::subjectAreaSaved,
            this, [self = QPointer<SubjectAreaController>(this)](const QString& name, const QString& domain_name) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Subject area saved: " << name.toStdString()
                                  << " in domain: " << domain_name.toStdString();
        self->handleEntitySaved();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle("New Subject Area");
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_table_20_regular.svg", iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void SubjectAreaController::showDetailWindow(
    const dq::domain::subject_area& subject_area) {

    const QString identifier = QString("%1|%2")
        .arg(QString::fromStdString(subject_area.name),
             QString::fromStdString(subject_area.domain_name));
    const QString key = build_window_key("details", identifier);

    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating detail window for: "
                               << subject_area.name;

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new SubjectAreaDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setCreateMode(false);
    detailDialog->setSubjectArea(subject_area);
    detailDialog->loadDomains();

    connect(detailDialog, &SubjectAreaDetailDialog::statusMessage,
            this, &SubjectAreaController::statusMessage);
    connect(detailDialog, &SubjectAreaDetailDialog::errorMessage,
            this, &SubjectAreaController::errorMessage);
    connect(detailDialog, &SubjectAreaDetailDialog::subjectAreaSaved,
            this, [self = QPointer<SubjectAreaController>(this)](const QString& name, const QString& domain_name) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Subject area saved: " << name.toStdString();
        self->handleEntitySaved();
    });
    connect(detailDialog, &SubjectAreaDetailDialog::subjectAreaDeleted,
            this, [self = QPointer<SubjectAreaController>(this), key](const QString& name, const QString& domain_name) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Subject area deleted: "
                                  << name.toStdString();
        self->handleEntityDeleted();
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Subject Area: %1")
        .arg(QString::fromStdString(subject_area.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_table_20_regular.svg", iconColor));

    track_window(key, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<SubjectAreaController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, key]() {
        if (self) {
            self->untrack_window(key);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

void SubjectAreaController::showHistoryWindow(const QString& name,
                                              const QString& domain_name) {
    BOOST_LOG_SEV(lg(), info) << "Opening history window for subject area: "
                              << name.toStdString()
                              << " in domain: " << domain_name.toStdString();

    const QString identifier = QString("%1|%2").arg(name, domain_name);
    const QString windowKey = build_window_key("history", identifier);

    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing history window for: "
                                  << name.toStdString();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Creating new history window for: "
                              << name.toStdString();
    const QColor iconColor(220, 220, 220);

    auto* historyDialog = new SubjectAreaHistoryDialog(
        name, domain_name, clientManager_, mainWindow_);

    connect(historyDialog, &SubjectAreaHistoryDialog::statusChanged,
            this, [self = QPointer<SubjectAreaController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(historyDialog, &SubjectAreaHistoryDialog::errorOccurred,
            this, [self = QPointer<SubjectAreaController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });
    connect(historyDialog, &SubjectAreaHistoryDialog::revertVersionRequested,
            this, &SubjectAreaController::onRevertVersion);
    connect(historyDialog, &SubjectAreaHistoryDialog::openVersionRequested,
            this, &SubjectAreaController::onOpenVersion);

    historyDialog->loadHistory();

    auto* historyWindow = new DetachableMdiSubWindow(mainWindow_);
    historyWindow->setAttribute(Qt::WA_DeleteOnClose);
    historyWindow->setWidget(historyDialog);
    historyWindow->setWindowTitle(
        QString("Subject Area History: %1").arg(name));
    historyWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, historyWindow);
    register_detachable_window(historyWindow);

    QPointer<SubjectAreaController> self = this;
    connect(historyWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    show_managed_window(historyWindow, listMdiSubWindow_);
}

void SubjectAreaController::onOpenVersion(
    const dq::domain::subject_area& subject_area, int versionNumber) {
    BOOST_LOG_SEV(lg(), info) << "Opening historical version " << versionNumber
                              << " for subject area: " << subject_area.name;

    const QString name = QString::fromStdString(subject_area.name);
    const QString domain_name = QString::fromStdString(subject_area.domain_name);
    const QString windowKey = build_window_key("version", QString("%1|%2_v%3")
        .arg(name, domain_name).arg(versionNumber));

    if (try_reuse_window(windowKey)) {
        BOOST_LOG_SEV(lg(), info) << "Reusing existing version window";
        return;
    }

    const QColor iconColor(220, 220, 220);

    auto* detailDialog = new SubjectAreaDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setSubjectArea(subject_area);
    detailDialog->setReadOnly(true);
    detailDialog->loadDomains();

    connect(detailDialog, &SubjectAreaDetailDialog::statusMessage,
            this, [self = QPointer<SubjectAreaController>(this)](const QString& message) {
        if (!self) return;
        emit self->statusMessage(message);
    });
    connect(detailDialog, &SubjectAreaDetailDialog::errorMessage,
            this, [self = QPointer<SubjectAreaController>(this)](const QString& message) {
        if (!self) return;
        emit self->errorMessage(message);
    });

    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Subject Area: %1 (Version %2)")
        .arg(name).arg(versionNumber));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_history_20_regular.svg", iconColor));

    track_window(windowKey, detailWindow);
    register_detachable_window(detailWindow);

    QPointer<SubjectAreaController> self = this;
    connect(detailWindow, &QObject::destroyed, this,
            [self, windowKey]() {
        if (self) {
            self->untrack_window(windowKey);
        }
    });

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_, QPoint(60, 60));
}

void SubjectAreaController::onRevertVersion(
    const dq::domain::subject_area& subject_area) {
    BOOST_LOG_SEV(lg(), info) << "Reverting subject area to version: "
                              << subject_area.version;

    auto* detailDialog = new SubjectAreaDetailDialog(mainWindow_);
    detailDialog->setClientManager(clientManager_);
    detailDialog->setUsername(username_.toStdString());
    detailDialog->setSubjectArea(subject_area);
    detailDialog->setCreateMode(false);
    detailDialog->loadDomains();

    connect(detailDialog, &SubjectAreaDetailDialog::statusMessage,
            this, &SubjectAreaController::statusMessage);
    connect(detailDialog, &SubjectAreaDetailDialog::errorMessage,
            this, &SubjectAreaController::errorMessage);
    connect(detailDialog, &SubjectAreaDetailDialog::subjectAreaSaved,
            this, [self = QPointer<SubjectAreaController>(this)](const QString& name, const QString& domain_name) {
        if (!self) return;
        BOOST_LOG_SEV(lg(), info) << "Subject area reverted: "
                                  << name.toStdString();
        emit self->statusMessage(
            QString("Subject area '%1' reverted successfully").arg(name));
        self->handleEntitySaved();
    });

    const QColor iconColor(220, 220, 220);
    auto* detailWindow = new DetachableMdiSubWindow(mainWindow_);
    detailWindow->setAttribute(Qt::WA_DeleteOnClose);
    detailWindow->setWidget(detailDialog);
    detailWindow->setWindowTitle(QString("Revert Subject Area: %1")
        .arg(QString::fromStdString(subject_area.name)));
    detailWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg",
        iconColor));

    register_detachable_window(detailWindow);

    connect_dialog_close(detailDialog, detailWindow);
    show_managed_window(detailWindow, listMdiSubWindow_);
}

}
