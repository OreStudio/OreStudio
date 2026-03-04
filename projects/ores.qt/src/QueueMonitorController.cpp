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
#include "ores.qt/QueueMonitorController.hpp"

#include <QPointer>
#include <QtConcurrent/QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/CreateQueueDialog.hpp"
#include "ores.qt/QueueChartWindow.hpp"
#include "ores.qt/QueueDetailDialog.hpp"
#include "ores.qt/QueueMonitorMdiWindow.hpp"
#include "ores.qt/DetachableMdiSubWindow.hpp"
#include "ores.mq/messaging/mq_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

QueueMonitorController::QueueMonitorController(
    QMainWindow* mainWindow,
    QMdiArea* mdiArea,
    ClientManager* clientManager,
    const QString& username,
    QObject* parent)
    : EntityController(mainWindow, mdiArea, clientManager, username,
          std::string_view{}, parent),
      listWindow_(nullptr),
      listMdiSubWindow_(nullptr) {

    BOOST_LOG_SEV(lg(), debug) << "QueueMonitorController created";
}

void QueueMonitorController::showListWindow() {
    BOOST_LOG_SEV(lg(), debug) << "showListWindow called";

    const QString key = build_window_key("list", "queues");
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing queue monitor window";
        return;
    }

    listWindow_ = new QueueMonitorMdiWindow(clientManager_);

    connect(listWindow_, &QueueMonitorMdiWindow::statusChanged,
            this, &QueueMonitorController::statusMessage);
    connect(listWindow_, &QueueMonitorMdiWindow::errorOccurred,
            this, &QueueMonitorController::errorMessage);
    connect(listWindow_, &QueueMonitorMdiWindow::viewChartRequested,
            this, &QueueMonitorController::onViewChartRequested);
    connect(listWindow_, &QueueMonitorMdiWindow::openDetailsRequested,
            this, &QueueMonitorController::onOpenDetailsRequested);
    connect(listWindow_, &QueueMonitorMdiWindow::createQueueRequested,
            this, &QueueMonitorController::onCreateQueueRequested);
    connect(listWindow_, &QueueMonitorMdiWindow::deleteQueueRequested,
            this, &QueueMonitorController::onDeleteQueueRequested);
    connect(listWindow_, &QueueMonitorMdiWindow::purgeQueueRequested,
            this, &QueueMonitorController::onPurgeQueueRequested);

    listMdiSubWindow_ = new DetachableMdiSubWindow(mainWindow_);
    listMdiSubWindow_->setWidget(listWindow_);
    listMdiSubWindow_->setWindowTitle(tr("Queue Monitor"));
    listMdiSubWindow_->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Server, IconUtils::DefaultIconColor));
    listMdiSubWindow_->setAttribute(Qt::WA_DeleteOnClose);
    listMdiSubWindow_->resize(listWindow_->sizeHint());

    mdiArea_->addSubWindow(listMdiSubWindow_);
    listMdiSubWindow_->show();

    track_window(key, listMdiSubWindow_);
    register_detachable_window(listMdiSubWindow_);

    connect(listMdiSubWindow_, &QObject::destroyed,
            this, [self = QPointer<QueueMonitorController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
        self->listWindow_ = nullptr;
        self->listMdiSubWindow_ = nullptr;
    });

    BOOST_LOG_SEV(lg(), debug) << "Queue monitor window created";
}

void QueueMonitorController::closeAllWindows() {
    BOOST_LOG_SEV(lg(), debug) << "closeAllWindows called";

    QList<QString> keys = managed_windows_.keys();
    for (const QString& key : keys) {
        if (auto* window = managed_windows_.value(key))
            window->close();
    }
    managed_windows_.clear();

    listWindow_ = nullptr;
    listMdiSubWindow_ = nullptr;
}

void QueueMonitorController::reloadListWindow() {
    if (listWindow_)
        listWindow_->reload();
}

EntityListMdiWindow* QueueMonitorController::listWindow() const {
    return listWindow_;
}

void QueueMonitorController::onViewChartRequested(const QString& queueId,
                                                  const QString& queueName) {
    BOOST_LOG_SEV(lg(), debug) << "View chart requested for: "
                               << queueName.toStdString();
    showChartWindow(queueId, queueName);
}

void QueueMonitorController::showChartWindow(const QString& queueId,
                                             const QString& queueName) {
    const QString key = build_window_key("chart", queueName);
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing chart window for: "
                                   << queueName.toStdString();
        return;
    }

    auto* chartWindow = new QueueChartWindow(queueId, queueName, clientManager_);

    connect(chartWindow, &QueueChartWindow::statusChanged,
            this, &QueueMonitorController::statusMessage);
    connect(chartWindow, &QueueChartWindow::errorOccurred,
            this, &QueueMonitorController::errorMessage);

    auto* subWindow = new DetachableMdiSubWindow(mainWindow_);
    subWindow->setWidget(chartWindow);
    subWindow->setWindowTitle(tr("Chart: %1").arg(queueName));
    subWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::Chart, IconUtils::DefaultIconColor));
    subWindow->setAttribute(Qt::WA_DeleteOnClose);
    subWindow->resize(chartWindow->sizeHint());

    track_window(key, subWindow);
    register_detachable_window(subWindow);

    connect(subWindow, &QObject::destroyed,
            this, [self = QPointer<QueueMonitorController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
    });

    show_managed_window(subWindow, listMdiSubWindow_, QPoint(60, 60));

    BOOST_LOG_SEV(lg(), debug) << "Chart window created for: "
                               << queueName.toStdString();
}

void QueueMonitorController::onOpenDetailsRequested(const QString& queueName) {
    BOOST_LOG_SEV(lg(), debug) << "Open details requested for: "
                               << queueName.toStdString();
    showDetailWindow(queueName);
}

void QueueMonitorController::showDetailWindow(const QString& queueName) {
    const QString key = build_window_key("details", queueName);
    if (try_reuse_window(key)) {
        BOOST_LOG_SEV(lg(), debug) << "Reusing existing detail window for: "
                                   << queueName.toStdString();
        return;
    }

    auto* detailWidget = new QueueDetailDialog(queueName, clientManager_);

    connect(detailWidget, &QueueDetailDialog::statusChanged,
            this, &QueueMonitorController::statusMessage);
    connect(detailWidget, &QueueDetailDialog::errorOccurred,
            this, &QueueMonitorController::errorMessage);

    auto* subWindow = new DetachableMdiSubWindow(mainWindow_);
    subWindow->setWidget(detailWidget);
    subWindow->setWindowTitle(tr("Queue: %1").arg(queueName));
    subWindow->setWindowIcon(IconUtils::createRecoloredIcon(
        Icon::DocumentTable, IconUtils::DefaultIconColor));
    subWindow->setAttribute(Qt::WA_DeleteOnClose);
    subWindow->resize(detailWidget->sizeHint());

    track_window(key, subWindow);
    register_detachable_window(subWindow);

    connect(subWindow, &QObject::destroyed,
            this, [self = QPointer<QueueMonitorController>(this), key]() {
        if (!self) return;
        self->untrack_window(key);
    });

    show_managed_window(subWindow, listMdiSubWindow_, QPoint(60, 60));

    BOOST_LOG_SEV(lg(), debug) << "Detail window created for: "
                               << queueName.toStdString();
}

void QueueMonitorController::onCreateQueueRequested() {
    BOOST_LOG_SEV(lg(), debug) << "Create queue requested";

    CreateQueueDialog dlg(mainWindow_);
    if (dlg.exec() != QDialog::Accepted)
        return;

    const QString queueName   = dlg.queueName();
    const QString scopeType   = dlg.scopeType();
    const QString queueType   = dlg.queueType();
    const QString description = dlg.description();

    if (queueName.isEmpty())
        return;

    struct CreateResult {
        bool success{false};
        QString error_message;
        QString error_details;
    };

    QPointer<QueueMonitorController> self = this;
    auto future = QtConcurrent::run(
            [self, queueName, scopeType, queueType, description]() -> CreateResult {
        return exception_helper::wrap_async_fetch<CreateResult>(
                [&]() -> CreateResult {
            if (!self || !self->clientManager_) {
                return {.success = false,
                        .error_message = tr("Controller destroyed"),
                        .error_details = {}};
            }

            mq::messaging::create_queue_request req;
            req.queue_name  = queueName.toStdString();
            req.scope_type  = scopeType.toStdString();
            req.queue_type  = queueType.toStdString();
            req.description = description.toStdString();

            auto result = self->clientManager_
                ->process_authenticated_request(std::move(req));

            if (!result) {
                return {.success = false,
                        .error_message = tr("Request failed"),
                        .error_details = {}};
            }

            if (!result->success) {
                return {.success = false,
                        .error_message = QString::fromStdString(result->message),
                        .error_details = {}};
            }

            return {.success = true, .error_message = {}, .error_details = {}};
        }, "create_queue");
    });

    auto* watcher = new QFutureWatcher<CreateResult>(this);
    connect(watcher, &QFutureWatcher<CreateResult>::finished,
            this, [self, watcher, queueName]() {
        const auto r = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (r.success) {
            BOOST_LOG_SEV(lg(), info) << "Queue created: "
                                      << queueName.toStdString();
            emit self->statusMessage(tr("Queue '%1' created").arg(queueName));
            self->reloadListWindow();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Create queue failed: "
                                       << r.error_message.toStdString();
            emit self->errorMessage(r.error_message);
            MessageBoxHelper::critical(self->mainWindow_,
                tr("Create Queue Failed"), r.error_message, r.error_details);
        }
    });
    watcher->setFuture(future);
}

void QueueMonitorController::onDeleteQueueRequested(const QString& queueName) {
    BOOST_LOG_SEV(lg(), debug) << "Delete queue requested: "
                               << queueName.toStdString();

    const auto reply = MessageBoxHelper::question(mainWindow_,
        tr("Delete Queue"),
        tr("Permanently delete queue '%1' and all its messages?").arg(queueName),
        QMessageBox::Yes | QMessageBox::No);
    if (reply != QMessageBox::Yes)
        return;

    struct DropResult {
        bool success{false};
        QString error_message;
        QString error_details;
    };

    QPointer<QueueMonitorController> self = this;
    auto future = QtConcurrent::run(
            [self, queueName]() -> DropResult {
        return exception_helper::wrap_async_fetch<DropResult>(
                [&]() -> DropResult {
            if (!self || !self->clientManager_) {
                return {.success = false,
                        .error_message = tr("Controller destroyed"),
                        .error_details = {}};
            }

            mq::messaging::drop_queue_request req;
            req.queue_name = queueName.toStdString();

            auto result = self->clientManager_
                ->process_authenticated_request(std::move(req));

            if (!result) {
                return {.success = false,
                        .error_message = tr("Request failed"),
                        .error_details = {}};
            }

            if (!result->success) {
                return {.success = false,
                        .error_message = QString::fromStdString(result->message),
                        .error_details = {}};
            }

            return {.success = true, .error_message = {}, .error_details = {}};
        }, "drop_queue");
    });

    auto* watcher = new QFutureWatcher<DropResult>(this);
    connect(watcher, &QFutureWatcher<DropResult>::finished,
            this, [self, watcher, queueName]() {
        const auto r = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (r.success) {
            BOOST_LOG_SEV(lg(), info) << "Queue deleted: "
                                      << queueName.toStdString();
            emit self->statusMessage(tr("Queue '%1' deleted").arg(queueName));
            self->reloadListWindow();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Delete queue failed: "
                                       << r.error_message.toStdString();
            emit self->errorMessage(r.error_message);
            MessageBoxHelper::critical(self->mainWindow_,
                tr("Delete Queue Failed"), r.error_message, r.error_details);
        }
    });
    watcher->setFuture(future);
}

void QueueMonitorController::onPurgeQueueRequested(const QString& queueName) {
    BOOST_LOG_SEV(lg(), debug) << "Purge queue requested: "
                               << queueName.toStdString();

    const auto reply = MessageBoxHelper::question(mainWindow_,
        tr("Purge Queue"),
        tr("Delete all messages from queue '%1'?").arg(queueName),
        QMessageBox::Yes | QMessageBox::No);
    if (reply != QMessageBox::Yes)
        return;

    struct PurgeResult {
        bool success{false};
        std::int64_t purged_count{0};
        QString error_message;
        QString error_details;
    };

    QPointer<QueueMonitorController> self = this;
    auto future = QtConcurrent::run(
            [self, queueName]() -> PurgeResult {
        return exception_helper::wrap_async_fetch<PurgeResult>(
                [&]() -> PurgeResult {
            if (!self || !self->clientManager_) {
                return {.success = false,
                        .error_message = tr("Controller destroyed"),
                        .error_details = {}};
            }

            mq::messaging::purge_queue_request req;
            req.queue_name = queueName.toStdString();

            auto result = self->clientManager_
                ->process_authenticated_request(std::move(req));

            if (!result) {
                return {.success = false,
                        .error_message = tr("Request failed"),
                        .error_details = {}};
            }

            if (!result->success) {
                return {.success = false,
                        .error_message = QString::fromStdString(result->message),
                        .error_details = {}};
            }

            return {.success = true,
                    .purged_count = result->purged_count,
                    .error_message = {}, .error_details = {}};
        }, "purge_queue");
    });

    auto* watcher = new QFutureWatcher<PurgeResult>(this);
    connect(watcher, &QFutureWatcher<PurgeResult>::finished,
            this, [self, watcher, queueName]() {
        const auto r = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (r.success) {
            BOOST_LOG_SEV(lg(), info) << "Queue purged: "
                                      << queueName.toStdString()
                                      << " (" << r.purged_count << " messages)";
            emit self->statusMessage(
                tr("Queue '%1' purged (%2 messages removed)")
                    .arg(queueName).arg(r.purged_count));
            self->reloadListWindow();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Purge queue failed: "
                                       << r.error_message.toStdString();
            emit self->errorMessage(r.error_message);
            MessageBoxHelper::critical(self->mainWindow_,
                tr("Purge Queue Failed"), r.error_message, r.error_details);
        }
    });
    watcher->setFuture(future);
}

}
