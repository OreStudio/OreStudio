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
#include "ores.qt/CurrencyHistoryDialog.hpp"

#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.risk/messaging/protocol.hpp"
#include "ores.comms/protocol/frame.hpp"
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QDateTime>
#include <QIcon>
#include <QScrollBar>

namespace ores::qt {

using comms::protocol::frame;
using comms::protocol::message_type;
using namespace ores::utility::log;

const QIcon& CurrencyHistoryDialog::getHistoryIcon() const {
    static const QIcon historyIcon(":/icons/ic_fluent_history_20_regular.svg");
    return historyIcon;
}

CurrencyHistoryDialog::CurrencyHistoryDialog(QString iso_code,
    std::shared_ptr<comms::net::client> client, QWidget* parent)
    : QWidget(parent), ui_(new Ui::CurrencyHistoryDialog),
      client_(std::move(client)), isoCode_(std::move(iso_code)) {

    BOOST_LOG_SEV(lg(), info) << "Creating currency history widget for: "
                              << isoCode_.toStdString();

    ui_->setupUi(this);

    connect(ui_->versionListWidget, &QTableWidget::currentCellChanged,
            this, [this](int currentRow, int, int, int) {
        onVersionSelected(currentRow);
    });

    ui_->versionListWidget->setAlternatingRowColors(true);
    ui_->versionListWidget->setSelectionMode(QAbstractItemView::SingleSelection);
    ui_->versionListWidget->setSelectionBehavior(QAbstractItemView::SelectRows);
    ui_->versionListWidget->resizeRowsToContents();

    QHeaderView* versionVerticalHeader = ui_->versionListWidget->verticalHeader();
    QHeaderView* versionHorizontalHeader = ui_->versionListWidget->horizontalHeader();
    versionVerticalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);
    versionHorizontalHeader->setSectionResizeMode(QHeaderView::ResizeToContents);

    ui_->changesTableWidget->horizontalHeader()->setStretchLastSection(true);
    ui_->changesTableWidget->setColumnWidth(0, 200);
    ui_->changesTableWidget->setColumnWidth(1, 200);
}

CurrencyHistoryDialog::~CurrencyHistoryDialog() {
    BOOST_LOG_SEV(lg(), info) << "Destroying currency history widget";

    // Disconnect and cancel any active QFutureWatcher objects
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void CurrencyHistoryDialog::loadHistory() {
    BOOST_LOG_SEV(lg(), info) << "Loading currency history for: "
                              << isoCode_.toStdString();

    risk::messaging::get_currency_history_request request{isoCode_.toStdString()};
    auto payload = request.serialize();

    frame request_frame(message_type::get_currency_history_request,
        0, std::move(payload)
    );

    using HistoryResult = std::expected<frame, std::string>;
    QPointer<CurrencyHistoryDialog> self = this;
    QFuture<HistoryResult> future =
        QtConcurrent::run([self, frame = std::move(request_frame)]() mutable -> HistoryResult {
        auto response_result = self->client_->send_request_sync(std::move(frame));
        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Could not obtain currency history: "
                                       << "Failed to communicate with server.";
            return std::unexpected("Failed to communicate with server");
        }
        return *response_result;
    });

    // Use watcher to handle results
    auto* watcher = new QFutureWatcher<HistoryResult>(self);
    connect(watcher, &QFutureWatcher<HistoryResult>::finished, self,
        [self, watcher]() {

        if (!self) return;
        auto result = watcher->result();
        watcher->deleteLater();

        if (!result) {
            self->onHistoryLoadError(QString::fromStdString(result.error()));
            return;
        }

        // Check if server sent an error_response instead
        if (result->header().type != message_type::get_currency_history_response) {
            self->onHistoryLoadError(
                QString("Server does not support currency history: received message type %1")
                .arg(static_cast<int>(result->header().type)));
            return;
        }

        auto response = risk::messaging::get_currency_history_response::
            deserialize(result->payload());

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Could not deserialise server response.";
            self->onHistoryLoadError("Invalid server response");
            return;
        }

        if (!response->success) {
            BOOST_LOG_SEV(lg(), error) << "Response was not success.";
            self->onHistoryLoadError(QString::fromStdString(response->message));
            return;
        }

        self->history_ = std::move(response->history);
        self->onHistoryLoaded();
    });

    watcher->setFuture(future);
}

void CurrencyHistoryDialog::onHistoryLoaded() {
    BOOST_LOG_SEV(lg(), info) << "History loaded successfully: "
                              << history_.versions.size() << " versions";

    const QIcon& cachedIcon = getHistoryIcon();
    ui_->versionListWidget->setRowCount(0);
    ui_->versionListWidget->setRowCount(history_.versions.size());

    for (int i = 0; i < static_cast<int>(history_.versions.size()); ++i) {
        const auto& version = history_.versions[i];

        auto* versionItem =
            new QTableWidgetItem(QString::number(version.version_number));
        auto* modifiedAtItem =
            new QTableWidgetItem(QString::fromStdString(version.modified_at));
        auto* modifiedByItem =
            new QTableWidgetItem(QString::fromStdString(version.modified_by));

        versionItem->setIcon(cachedIcon);

        ui_->versionListWidget->setItem(i, 0, versionItem);
        ui_->versionListWidget->setItem(i, 1, modifiedAtItem);
        ui_->versionListWidget->setItem(i, 2, modifiedByItem);
    }

    if (!history_.versions.empty())
        ui_->versionListWidget->selectRow(0);

    if (!history_.versions.empty()) {
        const auto& latest = history_.versions[0];
        ui_->titleLabel->setText(QString("Currency History: %1 - %2")
            .arg(isoCode_)
            .arg(QString::fromStdString(latest.data.name)));
    }

    emit statusChanged(QString("Loaded %1 versions")
        .arg(history_.versions.size()));
}

void CurrencyHistoryDialog::onHistoryLoadError(const QString& error_msg) {
    BOOST_LOG_SEV(lg(), error) << "Error loading history: "
                               << error_msg.toStdString();

    emit errorOccurred(QString("Failed to load currency history: %1")
        .arg(error_msg));
    MessageBoxHelper::critical(this, "History Load Error",
        QString("Failed to load currency history:\n%1")
        .arg(error_msg));
}

void CurrencyHistoryDialog::onVersionSelected(int index) {
    if (index < 0 || index >= static_cast<int>(history_.versions.size()))
        return;

    BOOST_LOG_SEV(lg(), trace) << "Version selected: " << index;

    displayChangesTab(index);
    displayFullDetailsTab(index);
}

void CurrencyHistoryDialog::displayChangesTab(int version_index) {
    ui_->changesTableWidget->setRowCount(0);

    if (version_index >= static_cast<int>(history_.versions.size()))
        return;

    const auto& current = history_.versions[version_index];

    // If this is the first (oldest) version, there's nothing to diff against so
    // leave the changes table empty
    if (version_index == static_cast<int>(history_.versions.size()) - 1)
        return;

    // Calculate diff with previous version
    const auto& previous = history_.versions[version_index + 1];
    auto diffs = calculateDiff(current, previous);

    ui_->changesTableWidget->setRowCount(diffs.size());

    for (int i = 0; i < diffs.size(); ++i) {
        const auto& [field, values] = diffs[i];
        const auto& [old_val, new_val] = values;

        auto* fieldItem = new QTableWidgetItem(field);
        auto* oldItem = new QTableWidgetItem(old_val);
        auto* newItem = new QTableWidgetItem(new_val);

        ui_->changesTableWidget->setItem(i, 0, fieldItem);
        ui_->changesTableWidget->setItem(i, 1, oldItem);
        ui_->changesTableWidget->setItem(i, 2, newItem);
    }
}

void CurrencyHistoryDialog::displayFullDetailsTab(int version_index) {
    if (version_index >= static_cast<int>(history_.versions.size()))
        return;

    const auto& version = history_.versions[version_index];
    const auto& data = version.data;

    ui_->isoCodeValue->setText(QString::fromStdString(data.iso_code));
    ui_->nameValue->setText(QString::fromStdString(data.name));
    ui_->numericCodeValue->setText(QString::fromStdString(data.numeric_code));
    ui_->symbolValue->setText(QString::fromStdString(data.symbol));
    ui_->fractionSymbolValue->setText(QString::fromStdString(data.fraction_symbol));
    ui_->fractionsPerUnitValue->setText(QString::number(data.fractions_per_unit));
    ui_->versionNumberValue->setText(QString::number(version.version_number));
    ui_->modifiedByValue->setText(QString::fromStdString(version.modified_by));
    ui_->modifiedAtValue->setText(QString::fromStdString(version.modified_at));
}

#define CHECK_DIFF_STRING(FIELD_NAME, FIELD) \
    if (current.data.FIELD != previous.data.FIELD) { \
        diffs.append({FIELD_NAME, { \
            QString::fromStdString(previous.data.FIELD), \
            QString::fromStdString(current.data.FIELD) \
        }}); \
    }

#define CHECK_DIFF_INT(FIELD_NAME, FIELD) \
    if (current.data.FIELD != previous.data.FIELD) { \
        diffs.append({FIELD_NAME, { \
            QString::number(previous.data.FIELD), \
            QString::number(current.data.FIELD) \
        }}); \
    }

CurrencyHistoryDialog::DiffResult CurrencyHistoryDialog::
calculateDiff(const risk::domain::currency_version& current,
    const risk::domain::currency_version& previous) {

    DiffResult diffs;

    // Compare string fields
    CHECK_DIFF_STRING("ISO Code", iso_code);
    CHECK_DIFF_STRING("Name", name);
    CHECK_DIFF_STRING("Numeric Code", numeric_code);
    CHECK_DIFF_STRING("Symbol", symbol);
    CHECK_DIFF_STRING("Fraction Symbol", fraction_symbol);
    CHECK_DIFF_STRING("Rounding Type", rounding_type);
    CHECK_DIFF_STRING("Format", format);
    CHECK_DIFF_STRING("Currency Type", currency_type);

    // Compare integer fields
    CHECK_DIFF_INT("Fractions Per Unit", fractions_per_unit);
    CHECK_DIFF_INT("Rounding Precision", rounding_precision);

    return diffs;
}

#undef CHECK_DIFF_STRING
#undef CHECK_DIFF_INT

QSize CurrencyHistoryDialog::sizeHint() const {
    // Call the base implementation first to get the minimum size required by
    // the layout manager and its content's size policies.
    QSize baseSize = QWidget::sizeHint();

    // Define a reasonable minimum size for a history dialog. This ensures the
    // two panes (Version List and Details/Changes) are comfortably visible.
    // These numbers are chosen to fit most content without excessive manual
    // calculation.
    const int minimumWidth = 900;
    const int minimumHeight = 600;

    // Return the maximum of the base size (to accommodate large text/UI
    // elements) and the defined minimum size.
    return { qMax(baseSize.width(), minimumWidth),
             qMax(baseSize.height(), minimumHeight) };
}

}
