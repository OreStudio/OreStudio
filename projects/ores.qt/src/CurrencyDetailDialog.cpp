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
#include "ores.risk/messaging/protocol.hpp"
#include "ores.risk/generators/currency_generator.hpp"
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

namespace {
    // Event type name for feature flag changes
    constexpr std::string_view feature_flag_event_name =
        eventing::domain::event_traits<variability::eventing::feature_flags_changed_event>::name;

    // Feature flag name for synthetic data generation
    constexpr std::string_view synthetic_generation_flag = "system.synthetic_data_generation";
}

CurrencyDetailDialog::CurrencyDetailDialog(QWidget* parent)
    : QWidget(parent), ui_(new Ui::CurrencyDetailDialog), isDirty_(false),
      isAddMode_(false), isReadOnly_(false), isStale_(false),
      historicalVersion_(0), flagButton_(nullptr),
      clientManager_(nullptr), imageCache_(nullptr) {

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
    revertAction_ = new QAction("Revert to this version", this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_arrow_clockwise_16_regular.svg", iconColor));
    revertAction_->setToolTip("Revert currency to this historical version");
    connect(revertAction_, &QAction::triggered, this,
        &CurrencyDetailDialog::onRevertClicked);
    toolBar_->addAction(revertAction_);
    revertAction_->setVisible(false);

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
            clientManager_->subscribeToEvent(std::string{feature_flag_event_name});
            // Defer visibility check to after event loop processes
            QTimer::singleShot(0, this, &CurrencyDetailDialog::updateGenerateActionVisibility);
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

CurrencyDetailDialog::~CurrencyDetailDialog() {
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void CurrencyDetailDialog::setCurrency(const risk::domain::currency& currency) {
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

    isDirty_ = false;
    flagChanged_ = false;
    emit isDirtyChanged(false);
    updateSaveResetButtonState();
    updateFlagDisplay();
}

risk::domain::currency CurrencyDetailDialog::getCurrency() const {
    risk::domain::currency currency = currentCurrency_;
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

    risk::domain::currency currency = getCurrency();

    QPointer<CurrencyDetailDialog> self = this;
    QFuture<FutureResult> future =
        QtConcurrent::run([self, currency]() -> FutureResult {
            if (!self) return {false, ""};
                BOOST_LOG_SEV(lg(), debug) << "Sending save currency request for: "
                                           << currency.iso_code;

                // Use single save_currency message for both create and update
                risk::messaging::save_currency_request request{currency};
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

                using risk::messaging::save_currency_response;
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

            emit self->statusMessage(QString("Successfully saved currency: %1")
                .arg(QString::fromStdString(currency.iso_code)));

            self->pendingImageId_.clear();

            self->isDirty_ = false;
            self->flagChanged_ = false;
            emit self->isDirtyChanged(false);
            self->updateSaveResetButtonState();

            if (self->isAddMode_) {
                emit self->currencyCreated(
                    QString::fromStdString(currency.iso_code));
            } else {
                emit self->currencyUpdated(
                    QString::fromStdString(currency.iso_code));
            }

            QWidget* parent = self->parentWidget();
            while (parent) {
                if (auto* mdiSubWindow = qobject_cast<QMdiSubWindow*>(parent)) {
                    QMetaObject::invokeMethod(mdiSubWindow, "close",
                        Qt::QueuedConnection);
                    break;
                }
                parent = parent->parentWidget();
            }
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
            risk::messaging::delete_currency_request request{{iso_code}};
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

            auto response = risk::messaging::delete_currency_response::deserialize(
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

    if (saveAction_)
        saveAction_->setEnabled(isDirty_);

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

    BOOST_LOG_SEV(lg(), debug) << "Checking feature flag for detail dialog: "
                               << synthetic_generation_flag;

    QPointer<CurrencyDetailDialog> self = this;
    QtConcurrent::run([self]() -> bool {
        if (!self || !self->clientManager_)
            return false;

        variability::messaging::list_feature_flags_request request;
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
        if (self && self->generateAction_) {
            self->generateAction_->setVisible(enabled);
            BOOST_LOG_SEV(lg(), debug) << "Generate action visibility in detail dialog set to: "
                                       << enabled;
        }
    });
}

void CurrencyDetailDialog::onGenerateClicked() {
    BOOST_LOG_SEV(lg(), debug) << "Generate clicked in detail dialog";

    try {
        auto currencies = risk::generators::generate_fictional_currencies();
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
    updateGenerateActionVisibility();
}

}
