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
#include "ores.qt/QueueDetailDialog.hpp"

#include <QPointer>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QFormLayout>
#include <QHeaderView>
#include <QtConcurrent>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.qt/MessageBoxHelper.hpp"

namespace ores::qt {

using namespace ores::logging;

QueueDetailDialog::QueueDetailDialog(const QString& queueName,
                                     ClientManager* clientManager,
                                     QWidget* parent)
    : QWidget(parent),
      queueName_(queueName),
      clientManager_(clientManager),
      toolbar_(nullptr),
      publishAction_(nullptr),
      getMessagesAction_(nullptr),
      deleteAction_(nullptr),
      tabWidget_(nullptr),
      publishTab_(nullptr),
      payloadEdit_(nullptr),
      delaySpinBox_(nullptr),
      messagesTab_(nullptr),
      modeCombo_(nullptr),
      countSpinBox_(nullptr),
      vtSpinBox_(nullptr),
      vtLabel_(nullptr),
      messagesTable_(nullptr),
      publishWatcher_(new QFutureWatcher<PublishResult>(this)),
      getWatcher_(new QFutureWatcher<GetMessagesResult>(this)) {

    connect(publishWatcher_, &QFutureWatcher<PublishResult>::finished,
            this, &QueueDetailDialog::onPublishDone);
    connect(getWatcher_, &QFutureWatcher<GetMessagesResult>::finished,
            this, &QueueDetailDialog::onGetMessagesDone);

    setupUi();
}

void QueueDetailDialog::setupUi() {
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    setupToolbar();
    layout->addWidget(toolbar_);

    tabWidget_ = new QTabWidget(this);
    setupPublishTab();
    setupMessagesTab();
    tabWidget_->addTab(publishTab_,  tr("Publish"));
    tabWidget_->addTab(messagesTab_, tr("Messages"));

    connect(tabWidget_, &QTabWidget::currentChanged, this, [this](int idx) {
        publishAction_->setVisible(idx == 0);
        getMessagesAction_->setVisible(idx == 1);
        deleteAction_->setVisible(idx == 1);
    });

    layout->addWidget(tabWidget_);
}

void QueueDetailDialog::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    publishAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Publish, IconUtils::DefaultIconColor),
        tr("Publish"));
    publishAction_->setToolTip(tr("Send the payload to the queue"));
    connect(publishAction_, &QAction::triggered,
            this, &QueueDetailDialog::onPublish);

    getMessagesAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::DocumentTable, IconUtils::DefaultIconColor),
        tr("Get Messages"));
    getMessagesAction_->setToolTip(tr("Read or pop messages from the queue"));
    getMessagesAction_->setVisible(false);
    connect(getMessagesAction_, &QAction::triggered,
            this, &QueueDetailDialog::onGetMessages);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected messages from the queue"));
    deleteAction_->setEnabled(false);
    deleteAction_->setVisible(false);
    connect(deleteAction_, &QAction::triggered,
            this, &QueueDetailDialog::onDeleteSelected);
}

void QueueDetailDialog::setupPublishTab() {
    publishTab_ = new QWidget();
    auto* layout = new QVBoxLayout(publishTab_);
    layout->setContentsMargins(8, 8, 8, 8);

    auto* formLayout = new QFormLayout();
    delaySpinBox_ = new QSpinBox();
    delaySpinBox_->setRange(0, 86400);
    delaySpinBox_->setSuffix(tr(" s"));
    delaySpinBox_->setToolTip(tr("Delay in seconds before the message becomes visible"));
    formLayout->addRow(tr("Delay:"), delaySpinBox_);
    layout->addLayout(formLayout);

    auto* payloadLabel = new QLabel(tr("Payload (JSON):"));
    layout->addWidget(payloadLabel);

    payloadEdit_ = new QPlainTextEdit();
    payloadEdit_->setPlaceholderText(tr("{ }"));
    QFont mono = payloadEdit_->font();
    mono.setFamily("Monospace");
    payloadEdit_->setFont(mono);
    layout->addWidget(payloadEdit_, 1);
}

void QueueDetailDialog::setupMessagesTab() {
    messagesTab_ = new QWidget();
    auto* layout = new QVBoxLayout(messagesTab_);
    layout->setContentsMargins(8, 8, 8, 8);
    layout->setSpacing(6);

    // Controls row
    auto* controls = new QHBoxLayout();
    controls->addWidget(new QLabel(tr("Mode:")));
    modeCombo_ = new QComboBox();
    modeCombo_->addItem(tr("Read (peek — non-destructive)"), QVariant::fromValue(int(GetMode::Read)));
    modeCombo_->addItem(tr("Pop (read + delete)"),          QVariant::fromValue(int(GetMode::Pop)));
    controls->addWidget(modeCombo_);

    controls->addSpacing(16);
    controls->addWidget(new QLabel(tr("Count:")));
    countSpinBox_ = new QSpinBox();
    countSpinBox_->setRange(1, 100);
    countSpinBox_->setValue(10);
    controls->addWidget(countSpinBox_);

    controls->addSpacing(16);
    vtLabel_ = new QLabel(tr("VT (s):"));
    controls->addWidget(vtLabel_);
    vtSpinBox_ = new QSpinBox();
    vtSpinBox_->setRange(0, 86400);
    vtSpinBox_->setValue(30);
    vtSpinBox_->setToolTip(tr("Visibility timeout — seconds the messages are hidden from other readers"));
    controls->addWidget(vtSpinBox_);

    controls->addStretch();
    layout->addLayout(controls);

    // Hide VT controls when mode is Pop (they are irrelevant)
    connect(modeCombo_, QOverload<int>::of(&QComboBox::currentIndexChanged),
            this, [this](int idx) {
        const bool is_read = (idx == 0);
        vtLabel_->setVisible(is_read);
        vtSpinBox_->setVisible(is_read);
    });

    auto* warning = new QLabel(
        tr("<small><i>Warning: Pop is destructive — messages cannot be recovered.</i></small>"));
    warning->setWordWrap(true);
    layout->addWidget(warning);

    // Results table
    messagesTable_ = new QTableWidget(0, 5, this);
    messagesTable_->setHorizontalHeaderLabels(
        {tr("ID"), tr("Read ct"), tr("Enqueued at"), tr("VT"), tr("Payload")});
    messagesTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    messagesTable_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    messagesTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    messagesTable_->setAlternatingRowColors(true);
    messagesTable_->verticalHeader()->setVisible(false);
    messagesTable_->horizontalHeader()->setStretchLastSection(true);
    messagesTable_->horizontalHeader()->setSectionResizeMode(
        QHeaderView::ResizeToContents);
    layout->addWidget(messagesTable_, 1);

    connect(messagesTable_, &QTableWidget::itemSelectionChanged,
            this, &QueueDetailDialog::updateDeleteAction);
}

void QueueDetailDialog::onPublish() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred(tr("Not connected to server"));
        return;
    }

    const QString payload = payloadEdit_->toPlainText().trimmed();
    if (payload.isEmpty()) {
        emit errorOccurred(tr("Payload is empty"));
        return;
    }

    publishAction_->setEnabled(false);
    emit statusChanged(tr("Publishing message to '%1'…").arg(queueName_));

    const QString queueName = queueName_;
    const int delay = delaySpinBox_->value();
    QPointer<QueueDetailDialog> self = this;

    QFuture<PublishResult> future = QtConcurrent::run(
        [self, queueName, payload, delay]() -> PublishResult {
            return exception_helper::wrap_async_fetch<PublishResult>(
                [&]() -> PublishResult {
                    if (!self || !self->clientManager_) {
                        return {.success = false, .msg_id = 0,
                                .error_message = "Dialog closed", .error_details = {}};
                    }

                    mq::messaging::send_message_request request;
                    request.queue_name = queueName.toStdString();
                    request.payload = payload.toStdString();
                    request.delay_seconds = delay;

                    auto result = self->clientManager_->process_authenticated_request(
                        std::move(request));

                    if (!result) {
                        return {.success = false, .msg_id = 0,
                                .error_message = tr("Request failed"),
                                .error_details = {}};
                    }

                    if (!result->success) {
                        return {.success = false, .msg_id = 0,
                                .error_message = QString::fromStdString(result->message),
                                .error_details = {}};
                    }

                    return {.success = true, .msg_id = result->msg_id,
                            .error_message = {}, .error_details = {}};
                }, "publish message");
        });

    publishWatcher_->setFuture(future);
}

void QueueDetailDialog::onPublishDone() {
    publishAction_->setEnabled(true);

    const auto result = publishWatcher_->result();
    if (result.success) {
        emit statusChanged(tr("Message %1 published to '%2'")
                           .arg(result.msg_id).arg(queueName_));
    } else {
        BOOST_LOG_SEV(lg(), error) << "Publish failed: "
                                   << result.error_message.toStdString();
        emit errorOccurred(result.error_message);
        MessageBoxHelper::critical(this, tr("Publish Error"),
                                   result.error_message, result.error_details);
    }
}

void QueueDetailDialog::onGetMessages() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        emit errorOccurred(tr("Not connected to server"));
        return;
    }

    getMessagesAction_->setEnabled(false);
    messagesTable_->setRowCount(0);

    const GetMode mode = static_cast<GetMode>(
        modeCombo_->currentData().toInt());
    const QString queueName = queueName_;
    const int count = countSpinBox_->value();
    const int vt = vtSpinBox_->value();

    emit statusChanged(tr("%1 messages from '%2'…")
                       .arg(mode == GetMode::Pop ? tr("Popping") : tr("Reading"))
                       .arg(queueName_));

    QPointer<QueueDetailDialog> self = this;

    QFuture<GetMessagesResult> future = QtConcurrent::run(
        [self, queueName, count, vt, mode]() -> GetMessagesResult {
            return exception_helper::wrap_async_fetch<GetMessagesResult>(
                [&]() -> GetMessagesResult {
                    if (!self || !self->clientManager_) {
                        return {.success = false, .messages = {}, .mode = mode,
                                .error_message = "Dialog closed", .error_details = {}};
                    }

                    std::vector<mq::messaging::queue_message> messages;

                    if (mode == GetMode::Pop) {
                        mq::messaging::pop_messages_request request;
                        request.queue_name = queueName.toStdString();
                        request.count = count;

                        auto result = self->clientManager_->process_authenticated_request(
                            std::move(request));

                        if (!result || !result->success) {
                            const QString err = result
                                ? QString::fromStdString(result->message)
                                : tr("Request failed");
                            return {.success = false, .messages = {}, .mode = mode,
                                    .error_message = err, .error_details = {}};
                        }
                        messages = std::move(result->messages);
                    } else {
                        mq::messaging::read_messages_request request;
                        request.queue_name = queueName.toStdString();
                        request.count = count;
                        request.vt_seconds = vt;

                        auto result = self->clientManager_->process_authenticated_request(
                            std::move(request));

                        if (!result || !result->success) {
                            const QString err = result
                                ? QString::fromStdString(result->message)
                                : tr("Request failed");
                            return {.success = false, .messages = {}, .mode = mode,
                                    .error_message = err, .error_details = {}};
                        }
                        messages = std::move(result->messages);
                    }

                    return {.success = true, .messages = std::move(messages),
                            .mode = mode, .error_message = {}, .error_details = {}};
                }, "get messages");
        });

    getWatcher_->setFuture(future);
}

void QueueDetailDialog::onGetMessagesDone() {
    getMessagesAction_->setEnabled(true);

    const auto result = getWatcher_->result();
    if (result.success) {
        populateMessagesTable(result.messages);
        emit statusChanged(tr("Fetched %1 message(s) from '%2'%3")
                           .arg(result.messages.size())
                           .arg(queueName_)
                           .arg(result.mode == GetMode::Pop ? tr(" (popped)") : QString{}));
    } else {
        BOOST_LOG_SEV(lg(), error) << "Get messages failed: "
                                   << result.error_message.toStdString();
        emit errorOccurred(result.error_message);
        MessageBoxHelper::critical(this, tr("Error"),
                                   result.error_message, result.error_details);
    }
}

void QueueDetailDialog::populateMessagesTable(
    const std::vector<mq::messaging::queue_message>& messages) {

    messagesTable_->setRowCount(static_cast<int>(messages.size()));
    for (int row = 0; row < static_cast<int>(messages.size()); ++row) {
        const auto& m = messages[row];
        messagesTable_->setItem(row, 0,
            new QTableWidgetItem(QString::number(m.msg_id)));
        messagesTable_->setItem(row, 1,
            new QTableWidgetItem(QString::number(m.read_ct)));
        messagesTable_->setItem(row, 2,
            new QTableWidgetItem(QString::fromStdString(m.enqueued_at)));
        messagesTable_->setItem(row, 3,
            new QTableWidgetItem(QString::fromStdString(m.vt)));
        // Store the full payload but truncate for display
        auto* payloadItem = new QTableWidgetItem(
            QString::fromStdString(m.payload));
        payloadItem->setData(Qt::UserRole, QString::fromStdString(m.payload));
        payloadItem->setToolTip(QString::fromStdString(m.payload));
        messagesTable_->setItem(row, 4, payloadItem);
    }
    updateDeleteAction();
}

void QueueDetailDialog::onDeleteSelected() {
    const auto selectedRows = messagesTable_->selectedItems();
    if (selectedRows.isEmpty()) return;

    // Collect unique row indices and their msg_ids
    QSet<int> rows;
    for (const auto* item : selectedRows)
        rows.insert(item->row());

    std::vector<std::int64_t> ids;
    for (int row : rows) {
        auto* idItem = messagesTable_->item(row, 0);
        if (idItem)
            ids.push_back(idItem->text().toLongLong());
    }

    if (ids.empty()) return;

    auto reply = MessageBoxHelper::question(this, tr("Delete Messages"),
        tr("Delete %n message(s) from queue '%1'?", "", static_cast<int>(ids.size()))
            .arg(queueName_));
    if (reply != QMessageBox::Yes) return;

    deleteAction_->setEnabled(false);

    const QString queueName = queueName_;
    QPointer<QueueDetailDialog> self = this;

    struct DeleteResult {
        bool success{false};
        int deleted_count{0};
        QString error_message;
        QString error_details;
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(this);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            this, [self, watcher]() {
        const auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        self->updateDeleteAction();
        if (result.success) {
            // Remove deleted rows from table (reverse order to keep indices valid)
            QList<int> toRemove;
            for (int row = 0; row < self->messagesTable_->rowCount(); ++row) {
                auto* item = self->messagesTable_->item(row, 0);
                if (item && item->isSelected())
                    toRemove.prepend(row);
            }
            for (int row : toRemove)
                self->messagesTable_->removeRow(row);

            emit self->statusChanged(
                tr("Deleted %1 message(s) from '%2'")
                    .arg(result.deleted_count).arg(self->queueName_));
        } else {
            emit self->errorOccurred(result.error_message);
            MessageBoxHelper::critical(self, tr("Delete Error"),
                                       result.error_message, result.error_details);
        }
    });

    watcher->setFuture(QtConcurrent::run(
        [self, queueName, ids]() -> DeleteResult {
            return exception_helper::wrap_async_fetch<DeleteResult>(
                [&]() -> DeleteResult {
                    if (!self || !self->clientManager_) {
                        return {.success = false, .deleted_count = 0,
                                .error_message = "Dialog closed", .error_details = {}};
                    }

                    mq::messaging::delete_messages_request request;
                    request.queue_name = queueName.toStdString();
                    request.msg_ids = ids;

                    auto result = self->clientManager_->process_authenticated_request(
                        std::move(request));

                    if (!result || !result->success) {
                        const QString err = result
                            ? QString::fromStdString(result->message)
                            : tr("Request failed");
                        return {.success = false, .deleted_count = 0,
                                .error_message = err, .error_details = {}};
                    }

                    return {.success = true, .deleted_count = result->deleted_count,
                            .error_message = {}, .error_details = {}};
                }, "delete messages");
        }));
}

void QueueDetailDialog::updateDeleteAction() {
    deleteAction_->setEnabled(
        messagesTable_->selectionModel() &&
        messagesTable_->selectionModel()->hasSelection());
}

}
