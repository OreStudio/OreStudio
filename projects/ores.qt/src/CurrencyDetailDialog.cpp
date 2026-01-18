/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/CurrencyDetailDialog.hpp"

#include <algorithm>
#include <random>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QToolBar>
#include <QIcon>
#include <QPixmap>
#include <QImage>
#include <QPainter>
#include <QMdiSubWindow>
#include <QMetaObject>
#include <QGroupBox>
#include <QTimer>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ui_CurrencyDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/MdiUtils.hpp"
#include "ores.qt/FlagSelectorDialog.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.dq/domain/change_reason_constants.hpp"
#include "ores.refdata/messaging/protocol.hpp"
#include "ores.refdata/generators/currency_generator.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.variability/eventing/feature_flags_changed_event.hpp"
#include "ores.variability/messaging/feature_flags_protocol.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::logging;
using FutureResult = std::pair<bool, std::string>;
namespace reason = dq::domain::change_reason_constants;

namespace {
    // Event type name for feature flag changes
    constexpr std::string_view feature_flag_event_name =
        eventing::domain::event_traits<variability::eventing::feature_flags_changed_event>::name;

    // Feature flag name for synthetic data generation
    constexpr std::string_view synthetic_generation_flag = "system.synthetic_data_generation";
}

CurrencyDetailDialog::CurrencyDetailDialog(QWidget* parent)
    : DetailDialogBase(parent), ui_(new Ui::CurrencyDetailDialog), isDirty_(false),
      isAddMode_(false), isReadOnly_(false), isStale_(false),
      historicalVersion_(0), flagButton_(nullptr),
      clientManager_(nullptr), imageCache_(nullptr), changeReasonCache_(nullptr),
      currentHistoryIndex_(0),
      firstVersionAction_(nullptr), prevVersionAction_(nullptr),
      nextVersionAction_(nullptr), lastVersionAction_(nullptr) {

    ui_->setupUi(this);

    // Create toolbar
    toolBar_ = new QToolBar(this);
    toolBar_->setMovable(false);
    toolBar_->setFloatable(false);

    // Define icon color (light gray for dark theme)
    const QColor iconColor(220, 220, 220);

    // Create Save action
    saveAction_ = new QAction("Save", this);
    saveAction_->setIcon(IconUtils::createRecoloredIcon(":/icons/ic_fluent_save_20_regular.svg",
            iconColor));
    saveAction_->setToolTip("Save changes");
    connect(saveAction_, &QAction::triggered, this,
        &CurrencyDetailDialog::onSaveClicked);
    toolBar_->addAction(saveAction_);

    // Create Delete action
    deleteAction_ = new QAction("Delete", this);
    deleteAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_delete_20_regular.svg", iconColor));
    deleteAction_->setToolTip("Delete currency");
    connect(deleteAction_, &QAction::triggered, this,
        &CurrencyDetailDialog::onDeleteClicked);
    toolBar_->addAction(deleteAction_);

    toolBar_->addSeparator();

    // Create Revert action (initially hidden)
    revertAction_ = new QAction("Revert", this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg", iconColor));
    revertAction_->setToolTip("Revert currency to this historical version");
    connect(revertAction_, &QAction::triggered, this,
        &CurrencyDetailDialog::onRevertClicked);
    toolBar_->addAction(revertAction_);
    revertAction_->setVisible(false);

    // Version navigation actions (initially hidden)
    toolBar_->addSeparator();

    firstVersionAction_ = new QAction("First", this);
    firstVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_previous_20_regular.svg", iconColor));
    firstVersionAction_->setToolTip(tr("First version"));
    connect(firstVersionAction_, &QAction::triggered, this,
        &CurrencyDetailDialog::onFirstVersionClicked);
    toolBar_->addAction(firstVersionAction_);
    firstVersionAction_->setVisible(false);

    prevVersionAction_ = new QAction("Previous", this);
    prevVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_left_20_regular.svg", iconColor));
    prevVersionAction_->setToolTip(tr("Previous version"));
    connect(prevVersionAction_, &QAction::triggered, this,
        &CurrencyDetailDialog::onPrevVersionClicked);
    toolBar_->addAction(prevVersionAction_);
    prevVersionAction_->setVisible(false);

    nextVersionAction_ = new QAction("Next", this);
    nextVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_right_20_regular.svg", iconColor));
    nextVersionAction_->setToolTip(tr("Next version"));
    connect(nextVersionAction_, &QAction::triggered, this,
        &CurrencyDetailDialog::onNextVersionClicked);
    toolBar_->addAction(nextVersionAction_);
    nextVersionAction_->setVisible(false);

    lastVersionAction_ = new QAction("Last", this);
    lastVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_next_20_regular.svg", iconColor));
    lastVersionAction_->setToolTip(tr("Last version"));
    connect(lastVersionAction_, &QAction::triggered, this,
        &CurrencyDetailDialog::onLastVersionClicked);
    toolBar_->addAction(lastVersionAction_);
    lastVersionAction_->setVisible(false);

    // Create Generate action (visibility controlled by feature flag)
    setupGenerateAction();

    // Add toolbar to the dialog's layout
    // Get the main layout from the UI
    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout)
        mainLayout->insertWidget(0, toolBar_);

    // Add clickable flag button after toolbar
    if (mainLayout) {
        auto* flagContainer = new QWidget(this);
        auto* flagLayout = new QHBoxLayout(flagContainer);
        flagLayout->setContentsMargins(0, 4, 0, 4);

        flagButton_ = new QPushButton(this);
        flagButton_->setFixedSize(52, 52);
        flagButton_->setIconSize(QSize(48, 48));
        flagButton_->setFlat(true);
        flagButton_->setStyleSheet("QPushButton { border: none; background: transparent; padding: 0px; } "
                                   "QPushButton:hover { background: rgba(255, 255, 255, 15); }");
        flagButton_->setCursor(Qt::PointingHandCursor);
        flagButton_->setToolTip(tr("Click to select flag"));
        connect(flagButton_, &QPushButton::clicked, this,
            &CurrencyDetailDialog::onSelectFlagClicked);
        flagLayout->addStretch();
        flagLayout->addWidget(flagButton_);
        flagLayout->addStretch();

        mainLayout->insertWidget(1, flagContainer);
    }

    // Connect signals for editable fields to detect changes
    connect(ui_->isoCodeEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->numericCodeEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->symbolEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->fractionSymbolEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->fractionsPerUnitSpinBox,
        QOverload<int>::of(&QSpinBox::valueChanged), this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->roundingTypeEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->roundingPrecisionSpinBox, QOverload<int>::of(
            &QSpinBox::valueChanged), this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->formatEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->currencyTypeEdit, &QLineEdit::textChanged, this,
        &CurrencyDetailDialog::onFieldChanged);

    // Initially disable save/reset buttons
    updateSaveResetButtonState();
}

void CurrencyDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;

    if (clientManager_) {
        // Connect to feature flag notifications for generate action visibility
        connect(clientManager_, &ClientManager::notificationReceived,
                this, &CurrencyDetailDialog::onFeatureFlagNotification);

        // Subscribe to feature flag events when connected/reconnected
        connect(clientManager_, &ClientManager::connected,
                this, &CurrencyDetailDialog::onConnectionEstablished);
        connect(clientManager_, &ClientManager::reconnected,
                this, &CurrencyDetailDialog::onConnectionEstablished);

        // If already connected, subscribe and check flag
        if (clientManager_->isConnected()) {
            onConnectionEstablished();
        }
    }
}

void CurrencyDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CurrencyDetailDialog::setImageCache(ImageCache* imageCache) {
    imageCache_ = imageCache;
    if (imageCache_) {
        connect(imageCache_, &ImageCache::currencyImageSet,
            this, &CurrencyDetailDialog::onCurrencyImageSet);
        // Connect to imagesLoaded (not currencyMappingsLoaded) because:
        // - currencyMappingsLoaded fires before icons are re-rendered
        // - imagesLoaded fires after icons are ready in currency_icons_
        connect(imageCache_, &ImageCache::imagesLoaded,
            this, &CurrencyDetailDialog::updateFlagDisplay);
    }
}

void CurrencyDetailDialog::setChangeReasonCache(ChangeReasonCache* changeReasonCache) {
    changeReasonCache_ = changeReasonCache;
}

CurrencyDetailDialog::~CurrencyDetailDialog() {
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void CurrencyDetailDialog::setCurrency(const refdata::domain::currency& currency) {
    currentCurrency_ = currency;
    isAddMode_ = currency.iso_code.empty();

    if (currency.image_id) {
        pendingImageId_ = QString::fromStdString(
            boost::uuids::to_string(*currency.image_id));
    } else if (imageCache_) {
        std::string noFlagId = imageCache_->getNoFlagImageId();
        if (!noFlagId.empty()) {
            pendingImageId_ = QString::fromStdString(noFlagId);
        } else {
            pendingImageId_.clear();
        }
    } else {
        pendingImageId_.clear();
    }

    ui_->isoCodeEdit->setReadOnly(!isAddMode_);
    ui_->isoCodeEdit->setText(QString::fromStdString(currency.iso_code));
    ui_->nameEdit->setText(QString::fromStdString(currency.name));
    ui_->numericCodeEdit->setText(QString::fromStdString(currency.numeric_code));
    ui_->symbolEdit->setText(QString::fromStdString(currency.symbol));
    ui_->fractionSymbolEdit->setText(QString::fromStdString(currency.fraction_symbol));
    ui_->fractionsPerUnitSpinBox->setValue(currency.fractions_per_unit);
    ui_->roundingTypeEdit->setText(QString::fromStdString(currency.rounding_type));
    ui_->roundingPrecisionSpinBox->setValue(currency.rounding_precision);
    ui_->formatEdit->setText(QString::fromStdString(currency.format));
    ui_->currencyTypeEdit->setText(QString::fromStdString(currency.currency_type));
    ui_->versionEdit->setText(QString::number(currency.version));
    ui_->recordedByEdit->setText(QString::fromStdString(currency.recorded_by));
    ui_->recordedAtEdit->setText(QString::fromStdString(
        platform::time::datetime::format_time_point(currency.recorded_at)));
    ui_->changeReasonEdit->setText(QString::fromStdString(currency.change_reason_code));
    ui_->commentaryEdit->setText(QString::fromStdString(currency.change_commentary));

    isDirty_ = false;
    flagChanged_ = false;
    emit isDirtyChanged(false);
    updateSaveResetButtonState();
    updateFlagDisplay();
}

refdata::domain::currency CurrencyDetailDialog::getCurrency() const {
    refdata::domain::currency currency = currentCurrency_;
    currency.iso_code = ui_->isoCodeEdit->text().toStdString();
    currency.name = ui_->nameEdit->text().toStdString();
    currency.numeric_code = ui_->numericCodeEdit->text().toStdString();
    currency.symbol = ui_->symbolEdit->text().toStdString();
    currency.fraction_symbol = ui_->fractionSymbolEdit->text().toStdString();
    currency.fractions_per_unit = ui_->fractionsPerUnitSpinBox->value();
    currency.rounding_type = ui_->roundingTypeEdit->text().toStdString();
    currency.rounding_precision = ui_->roundingPrecisionSpinBox->value();
    currency.format = ui_->formatEdit->text().toStdString();
    currency.currency_type = ui_->currencyTypeEdit->text().toStdString();
    currency.recorded_by = username_.empty() ? "qt_user" : username_;

    if (!pendingImageId_.isEmpty()) {
        boost::uuids::string_generator gen;
        currency.image_id = gen(pendingImageId_.toStdString());
    }

    return currency;
}

void CurrencyDetailDialog::clearDialog() {
    ui_->isoCodeEdit->clear();
    ui_->nameEdit->clear();
    ui_->numericCodeEdit->clear();
    ui_->symbolEdit->clear();
    ui_->fractionSymbolEdit->clear();
    ui_->fractionsPerUnitSpinBox->clear();
    ui_->roundingTypeEdit->clear();
    ui_->roundingPrecisionSpinBox->clear();
    ui_->formatEdit->clear();
    ui_->currencyTypeEdit->clear();
    ui_->versionEdit->clear();
    ui_->recordedByEdit->clear();
    ui_->recordedAtEdit->clear();
    ui_->changeReasonEdit->clear();
    ui_->commentaryEdit->clear();
    pendingImageId_.clear();

    isDirty_ = false;
    flagChanged_ = false;
    emit isDirtyChanged(false);
    updateSaveResetButtonState();
}

void CurrencyDetailDialog::save() {
    onSaveClicked();
}

void CurrencyDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Save clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Save clicked for currency: "
                               << currentCurrency_.iso_code;

    refdata::domain::currency currency = getCurrency();

    // For updates (not creates), require change reason
    if (!isAddMode_) {
        if (!changeReasonCache_ || !changeReasonCache_->isLoaded()) {
            BOOST_LOG_SEV(lg(), warn) << "Change reasons not loaded, cannot save.";
            emit errorMessage("Change reasons not loaded. Please try again.");
            return;
        }

        // Get reasons for the "common" category that apply to amendments
        auto reasons = changeReasonCache_->getReasonsForAmend(
            std::string{reason::categories::common});
        if (reasons.empty()) {
            BOOST_LOG_SEV(lg(), warn) << "No change reasons available for common category.";
            emit errorMessage("No change reasons available. Please contact administrator.");
            return;
        }

        ChangeReasonDialog dialog(reasons, ChangeReasonDialog::OperationType::Amend,
            isDirty_, this);
        if (dialog.exec() != QDialog::Accepted) {
            BOOST_LOG_SEV(lg(), debug) << "Save cancelled - change reason dialog rejected.";
            return;
        }

        // Set the change reason on the currency
        currency.change_reason_code = dialog.selectedReasonCode();
        currency.change_commentary = dialog.commentary();

        BOOST_LOG_SEV(lg(), debug) << "Change reason selected: "
                                   << currency.change_reason_code
                                   << ", commentary: " << currency.change_commentary;
    }

    QPointer<CurrencyDetailDialog> self = this;
    QFuture<FutureResult> future =
        QtConcurrent::run([self, currency]() -> FutureResult {
            if (!self) return {false, ""};
                BOOST_LOG_SEV(lg(), debug) << "Sending save currency request for: "
                                           << currency.iso_code;

                // Use single save_currency message for both create and update
                refdata::messaging::save_currency_request request{currency};
                auto payload = request.serialize();
                frame request_frame = frame(message_type::save_currency_request,
                    0, std::move(payload));

                auto response_result =
                    self->clientManager_->sendRequest(std::move(request_frame));

                if (!response_result)
                    return {false, "Failed to communicate with server"};

                BOOST_LOG_SEV(lg(), debug) << "Received save currency response.";

                // Decompress payload
                auto payload_result = response_result->decompressed_payload();
                if (!payload_result)
                    return {false, "Failed to decompress server response"};

                using refdata::messaging::save_currency_response;
                auto response = save_currency_response::
                    deserialize(*payload_result);

                bool result = false;
                std::string message = "Invalid server response";
                if (response) {
                    result = response->success;
                    message = response->message;
                }

                return {result, message};
            });

    auto* watcher = new QFutureWatcher<FutureResult>(self);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, currency]() {

        if (!self)
            return;

        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Currency saved successfully.";

            self->pendingImageId_.clear();

            self->isDirty_ = false;
            self->flagChanged_ = false;
            emit self->isDirtyChanged(false);
            self->updateSaveResetButtonState();

            QString code = QString::fromStdString(currency.iso_code);
            if (self->isAddMode_) {
                emit self->currencyCreated(code);
            } else {
                emit self->currencyUpdated(code);
            }

            self->notifySaveSuccess(tr("Currency '%1' saved").arg(code));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Currency save failed: " << message;
            emit self->errorMessage(QString("Failed to save currency: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Save Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

void CurrencyDetailDialog::onResetClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Reset clicked for currency: "
                               << currentCurrency_.iso_code;
    setCurrency(currentCurrency_);
}

void CurrencyDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete request for currency: "
                               << currentCurrency_.iso_code;

    auto reply =
        MessageBoxHelper::question(this, "Delete Currency",
        QString("Are you sure you want to delete currency '%1' (%2)?")
        .arg(QString::fromStdString(currentCurrency_.name))
        .arg(QString::fromStdString(currentCurrency_.iso_code)),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<CurrencyDetailDialog> self = this;
    const std::string iso_code = currentCurrency_.iso_code;
    QFuture<FutureResult> future =
        QtConcurrent::run([self, iso_code]() -> FutureResult {
            if (!self) return {false, {}};
            BOOST_LOG_SEV(lg(), debug) << "Sending delete currency request for: "
                                       << iso_code;

            // Create batch request with single ISO code
            refdata::messaging::delete_currency_request request{{iso_code}};
            auto payload = request.serialize();

            frame request_frame(message_type::delete_currency_request,
                    0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to communicate with server.";
                return {false, "Failed to communicate with server."};
            }

            BOOST_LOG_SEV(lg(), debug) << "Received delete currency response.";

            // Decompress payload
            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to decompress server response";
                return {false, "Failed to decompress server response"};
            }

            auto response = refdata::messaging::delete_currency_response::deserialize(
                *payload_result);

            if (!response) {
                BOOST_LOG_SEV(lg(), error) << "Invalid server response";
                return {false, "Invalid server response"};
            }

            // Extract result for the single currency
            if (response->results.empty()) {
                BOOST_LOG_SEV(lg(), error) << "Empty results in response";
                return {false, "Empty results in response"};
            }

            return {response->results[0].success, response->results[0].message};
        });


    auto* watcher = new QFutureWatcher<FutureResult>(self);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, iso_code]() {

        BOOST_LOG_SEV(lg(), debug) << "Received currencies delete result.";
        if (!self) return;
        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Currency deleted successfully.";
            emit self->statusMessage(QString("Successfully deleted currency: %1")
                .arg(QString::fromStdString(iso_code)));
            emit self->currencyDeleted(QString::fromStdString(iso_code));
            self->clearDialog();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Currency deletion failed: "
                                       << message;
            emit self->errorMessage(QString("Failed to delete currency: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Delete Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

void CurrencyDetailDialog::onFieldChanged() {
    if (isReadOnly_)
        return;

    isDirty_ = true;
    emit isDirtyChanged(true);
    updateSaveResetButtonState();
}

void CurrencyDetailDialog::onRevertClicked() {
    BOOST_LOG_SEV(lg(), info) << "Revert clicked for historical version "
                              << historicalVersion_;

    // Confirm with user
    auto reply = MessageBoxHelper::question(this, "Revert Currency",
        QString("Are you sure you want to revert '%1' to version %2?\n\n"
                "This will create a new version with the data from version %2.")
            .arg(QString::fromStdString(currentCurrency_.iso_code))
            .arg(historicalVersion_),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Revert cancelled by user";
        return;
    }

    emit revertRequested(currentCurrency_);
}

void CurrencyDetailDialog::setReadOnly(bool readOnly, int versionNumber) {
    isReadOnly_ = readOnly;
    historicalVersion_ = versionNumber;

    BOOST_LOG_SEV(lg(), debug) << "Setting read-only mode: " << readOnly
                               << ", version: " << versionNumber;

    setFieldsReadOnly(readOnly);

    // Update toolbar visibility
    if (saveAction_)
        saveAction_->setVisible(!readOnly);

    if (deleteAction_)
        deleteAction_->setVisible(!readOnly);

    if (flagButton_)
        flagButton_->setEnabled(!readOnly);

    if (revertAction_)
        revertAction_->setVisible(readOnly);

    // Hide generate action in read-only mode (history view)
    if (generateAction_)
        generateAction_->setVisible(false);

    updateSaveResetButtonState();
}

void CurrencyDetailDialog::setFieldsReadOnly(bool readOnly) {
    ui_->isoCodeEdit->setReadOnly(readOnly);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->numericCodeEdit->setReadOnly(readOnly);
    ui_->symbolEdit->setReadOnly(readOnly);
    ui_->fractionSymbolEdit->setReadOnly(readOnly);
    ui_->fractionsPerUnitSpinBox->setReadOnly(readOnly);
    ui_->roundingTypeEdit->setReadOnly(readOnly);
    ui_->roundingPrecisionSpinBox->setReadOnly(readOnly);
    ui_->formatEdit->setReadOnly(readOnly);
    ui_->currencyTypeEdit->setReadOnly(readOnly);
}

void CurrencyDetailDialog::updateSaveResetButtonState() {
    if (isReadOnly_) {
        if (saveAction_)
            saveAction_->setEnabled(false);
        if (deleteAction_)
            deleteAction_->setEnabled(false);
        if (revertAction_)
            revertAction_->setEnabled(true);
        return;
    }

    // In add mode, only enable save when dirty
    // In edit mode, always enable save (for "touch" operations that update timestamp)
    if (saveAction_)
        saveAction_->setEnabled(isAddMode_ ? isDirty_ : true);

    if (deleteAction_)
        deleteAction_->setEnabled(!isAddMode_);
}

void CurrencyDetailDialog::markAsStale() {
    if (isStale_)
        return;

    isStale_ = true;
    BOOST_LOG_SEV(lg(), info) << "Currency detail data marked as stale for: "
                              << currentCurrency_.iso_code;

    MdiUtils::markParentWindowAsStale(this);

    emit statusMessage(QString("Currency %1 has been modified on the server")
        .arg(QString::fromStdString(currentCurrency_.iso_code)));
}

QString CurrencyDetailDialog::isoCode() const {
    return QString::fromStdString(currentCurrency_.iso_code);
}

void CurrencyDetailDialog::onSelectFlagClicked() {
    if (!imageCache_) {
        BOOST_LOG_SEV(lg(), warn) << "Flag clicked but no image cache available.";
        emit errorMessage("Image cache not available");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Opening flag selector for: "
                               << currentCurrency_.iso_code;

    // Get current image ID - use pending if set, otherwise from the currency
    QString currentImageId = pendingImageId_.isEmpty()
        ? (currentCurrency_.image_id
            ? QString::fromStdString(boost::uuids::to_string(*currentCurrency_.image_id))
            : QString())
        : pendingImageId_;

    FlagSelectorDialog dialog(imageCache_, currentImageId, this);
    if (dialog.exec() == QDialog::Accepted) {
        QString selectedImageId = dialog.selectedImageId();
        BOOST_LOG_SEV(lg(), debug) << "Selected flag image: "
                                   << selectedImageId.toStdString();

        // Store the selection locally - will be persisted on Save
        pendingImageId_ = selectedImageId;
        flagChanged_ = true;

        // Update display to show the new selection
        updateFlagDisplay();

        // Mark as dirty so Save button is enabled
        if (!isDirty_) {
            isDirty_ = true;
            emit isDirtyChanged(true);
            updateSaveResetButtonState();
        }

        emit statusMessage(tr("Flag changed. Click Save to apply."));
    }
}

void CurrencyDetailDialog::onCurrencyImageSet(const QString& iso_code,
    bool success, const QString& message) {

    if (iso_code.toStdString() != currentCurrency_.iso_code) {
        return;  // Not our currency
    }

    if (success) {
        BOOST_LOG_SEV(lg(), info) << "Flag updated successfully for: "
                                  << currentCurrency_.iso_code;
        emit statusMessage(tr("Flag updated for %1")
            .arg(QString::fromStdString(currentCurrency_.iso_code)));
        updateFlagDisplay();
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to update flag for "
                                   << currentCurrency_.iso_code << ": "
                                   << message.toStdString();
        emit errorMessage(tr("Failed to update flag: %1").arg(message));
        MessageBoxHelper::critical(this, "Flag Update Failed", message);
    }
    pendingImageId_.clear();
}

void CurrencyDetailDialog::updateFlagDisplay() {
    if (!flagButton_)
        return;

    // Update styling based on changed state
    if (flagChanged_) {
        // Show orange border to indicate unsaved change
        flagButton_->setStyleSheet("QPushButton { border: 2px solid orange; background: transparent; padding: 0px; border-radius: 4px; } "
                                   "QPushButton:hover { background: rgba(255, 255, 255, 15); }");
        flagButton_->setToolTip(tr("Flag changed (unsaved)"));
    } else {
        // Normal transparent style
        flagButton_->setStyleSheet("QPushButton { border: none; background: transparent; padding: 0px; } "
                                   "QPushButton:hover { background: rgba(255, 255, 255, 15); }");
        flagButton_->setToolTip(tr("Click to select flag"));
    }

    if (!imageCache_) {
        // No image cache available - use the no-flag placeholder from cache
        // when it becomes available
        return;
    }

    // Determine which image ID to show
    QString imageIdToShow;

    if (flagChanged_) {
        // If changed, show pending ID (even if empty - meaning cleared)
        imageIdToShow = pendingImageId_;
    } else if (currentCurrency_.image_id) {
        // Get image_id from the currency domain object
        imageIdToShow = QString::fromStdString(
            boost::uuids::to_string(*currentCurrency_.image_id));
    }

    // If we have an ID, try to get the icon
    if (!imageIdToShow.isEmpty()) {
        QIcon icon = imageCache_->getIcon(imageIdToShow.toStdString());
        if (!icon.isNull()) {
            flagButton_->setIcon(icon);
            return;
        }
    }

    // Default to "no-flag" placeholder icon
    QIcon noFlagIcon = imageCache_->getNoFlagIcon();
    if (!noFlagIcon.isNull()) {
        flagButton_->setIcon(noFlagIcon);
    }
}

void CurrencyDetailDialog::showVersionNavActions(bool visible) {
    if (firstVersionAction_)
        firstVersionAction_->setVisible(visible);
    if (prevVersionAction_)
        prevVersionAction_->setVisible(visible);
    if (nextVersionAction_)
        nextVersionAction_->setVisible(visible);
    if (lastVersionAction_)
        lastVersionAction_->setVisible(visible);
}

void CurrencyDetailDialog::setHistory(
    const refdata::domain::currency_version_history& history, int versionNumber) {
    BOOST_LOG_SEV(lg(), debug) << "Setting history with " << history.versions.size()
                               << " versions, displaying version " << versionNumber;

    history_ = history;

    // Find index of the requested version (history is newest-first)
    currentHistoryIndex_ = 0;
    for (size_t i = 0; i < history_.versions.size(); ++i) {
        if (history_.versions[i].version_number == versionNumber) {
            currentHistoryIndex_ = static_cast<int>(i);
            break;
        }
    }

    displayCurrentVersion();
    showVersionNavActions(true);
}

void CurrencyDetailDialog::displayCurrentVersion() {
    if (history_.versions.empty() || currentHistoryIndex_ < 0 ||
        currentHistoryIndex_ >= static_cast<int>(history_.versions.size())) {
        return;
    }

    const auto& version = history_.versions[currentHistoryIndex_];

    // Display the currency data
    setCurrency(version.data);
    setReadOnly(true, version.version_number);

    // Update button states
    updateVersionNavButtonStates();

    // Gray out flag in history view - all versions are read-only
    if (flagButton_) {
        flagButton_->setEnabled(false);
        flagButton_->setToolTip(tr("Historical version - flag display only"));
    }

    // Update window title with short version format
    QWidget* parent = parentWidget();
    while (parent) {
        if (auto* mdiSubWindow = qobject_cast<QMdiSubWindow*>(parent)) {
            mdiSubWindow->setWindowTitle(QString("Currency: %1 v%2")
                .arg(QString::fromStdString(version.data.iso_code))
                .arg(version.version_number));
            break;
        }
        parent = parent->parentWidget();
    }
}

void CurrencyDetailDialog::updateVersionNavButtonStates() {
    if (history_.versions.empty()) {
        if (firstVersionAction_) firstVersionAction_->setEnabled(false);
        if (prevVersionAction_) prevVersionAction_->setEnabled(false);
        if (nextVersionAction_) nextVersionAction_->setEnabled(false);
        if (lastVersionAction_) lastVersionAction_->setEnabled(false);
        return;
    }

    bool atOldest = (currentHistoryIndex_ == static_cast<int>(history_.versions.size()) - 1);
    bool atNewest = (currentHistoryIndex_ == 0);

    if (firstVersionAction_) firstVersionAction_->setEnabled(!atOldest);  // Go to oldest
    if (prevVersionAction_) prevVersionAction_->setEnabled(!atOldest);   // Go to older
    if (nextVersionAction_) nextVersionAction_->setEnabled(!atNewest);   // Go to newer
    if (lastVersionAction_) lastVersionAction_->setEnabled(!atNewest);   // Go to latest
}

void CurrencyDetailDialog::onFirstVersionClicked() {
    if (history_.versions.empty()) return;

    BOOST_LOG_SEV(lg(), debug) << "Navigating to first (oldest) version";
    currentHistoryIndex_ = static_cast<int>(history_.versions.size()) - 1;
    displayCurrentVersion();
}

void CurrencyDetailDialog::onPrevVersionClicked() {
    if (history_.versions.empty()) return;

    if (currentHistoryIndex_ < static_cast<int>(history_.versions.size()) - 1) {
        BOOST_LOG_SEV(lg(), debug) << "Navigating to previous (older) version";
        ++currentHistoryIndex_;
        displayCurrentVersion();
    }
}

void CurrencyDetailDialog::onNextVersionClicked() {
    if (history_.versions.empty()) return;

    if (currentHistoryIndex_ > 0) {
        BOOST_LOG_SEV(lg(), debug) << "Navigating to next (newer) version";
        --currentHistoryIndex_;
        displayCurrentVersion();
    }
}

void CurrencyDetailDialog::onLastVersionClicked() {
    if (history_.versions.empty()) return;

    BOOST_LOG_SEV(lg(), debug) << "Navigating to last (latest) version";
    currentHistoryIndex_ = 0;
    displayCurrentVersion();
}

void CurrencyDetailDialog::setupGenerateAction() {
    const QColor iconColor(220, 220, 220);

    generateAction_ = new QAction("Generate", this);
    generateAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_wand_20_regular.svg", iconColor));
    generateAction_->setToolTip("Fill fields with synthetic test data");
    connect(generateAction_, &QAction::triggered, this,
        &CurrencyDetailDialog::onGenerateClicked);
    toolBar_->addAction(generateAction_);

    // Initially hidden - will be shown if feature flag is enabled
    generateAction_->setVisible(false);
}

void CurrencyDetailDialog::updateGenerateActionVisibility() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        generateAction_->setVisible(false);
        return;
    }

    // Don't show in read-only mode (history view)
    if (isReadOnly_) {
        generateAction_->setVisible(false);
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Checking feature flag for detail dialog: "
                               << synthetic_generation_flag;

    QPointer<CurrencyDetailDialog> self = this;
    QtConcurrent::run([self]() -> bool {
        if (!self || !self->clientManager_)
            return false;

        variability::messaging::get_feature_flags_request request;
        auto result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!result) {
            BOOST_LOG_SEV(lg(), debug) << "Feature flags request failed";
            return false;
        }

        // Find our specific flag
        auto it = std::find_if(result->feature_flags.begin(), result->feature_flags.end(),
            [](const auto& flag) {
                return flag.name == synthetic_generation_flag;
            });

        if (it == result->feature_flags.end()) {
            BOOST_LOG_SEV(lg(), debug) << "Feature flag not found: " << synthetic_generation_flag;
            return false;
        }

        return it->enabled;
    }).then(this, [self](bool enabled) {
        if (self && self->generateAction_ && !self->isReadOnly_) {
            self->generateAction_->setVisible(enabled);
            BOOST_LOG_SEV(lg(), debug) << "Generate action visibility in detail dialog set to: "
                                       << enabled;
        }
    });
}

void CurrencyDetailDialog::onGenerateClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Generate clicked in detail dialog";

    try {
        auto currencies = refdata::generators::generate_fictional_currencies();
        static std::random_device rd;
        static std::mt19937 gen(rd());
        std::uniform_int_distribution<std::size_t> dist(0, currencies.size() - 1);
        const auto& currency = currencies[dist(gen)];

        // Only fill ISO code in add mode - in edit mode it's the primary key
        if (isAddMode_) {
            ui_->isoCodeEdit->setText(QString::fromStdString(currency.iso_code));
        }

        // Fill all other fields with generated data
        ui_->nameEdit->setText(QString::fromStdString(currency.name));
        ui_->numericCodeEdit->setText(QString::fromStdString(currency.numeric_code));
        ui_->symbolEdit->setText(QString::fromStdString(currency.symbol));
        ui_->fractionSymbolEdit->setText(QString::fromStdString(currency.fraction_symbol));
        ui_->fractionsPerUnitSpinBox->setValue(currency.fractions_per_unit);
        ui_->roundingTypeEdit->setText(QString::fromStdString(currency.rounding_type));
        ui_->roundingPrecisionSpinBox->setValue(currency.rounding_precision);
        ui_->formatEdit->setText(QString::fromStdString(currency.format));
        ui_->currencyTypeEdit->setText(QString::fromStdString(currency.currency_type));

        // Mark as dirty
        isDirty_ = true;
        emit isDirtyChanged(true);
        updateSaveResetButtonState();

        emit statusMessage("Generated synthetic currency data - modify as needed and save");

        BOOST_LOG_SEV(lg(), info) << "Filled fields with generated currency: "
                                  << currency.iso_code;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Error generating currency: " << e.what();
        MessageBoxHelper::critical(this, "Generation Error",
            QString("Failed to generate currency data: %1").arg(e.what()));
    }
}

void CurrencyDetailDialog::onFeatureFlagNotification(
    const QString& eventType, const QDateTime& timestamp,
    const QStringList& entityIds) {

    // Check if this is a feature flag change event
    if (eventType != QString::fromStdString(std::string{feature_flag_event_name})) {
        return;
    }

    // Check if our flag was affected
    QString ourFlag = QString::fromStdString(std::string{synthetic_generation_flag});
    if (!entityIds.isEmpty() && !entityIds.contains(ourFlag)) {
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Feature flag notification received in detail dialog";
    updateGenerateActionVisibility();
}

void CurrencyDetailDialog::onConnectionEstablished() {
    clientManager_->subscribeToEvent(std::string{feature_flag_event_name});

    // Use QTimer to delay the visibility check until after event loop processes
    QTimer::singleShot(100, this, &CurrencyDetailDialog::updateGenerateActionVisibility);
}

}
