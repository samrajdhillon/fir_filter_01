filter_coeff  = [0.25333 0.123561 0.823561 -0.258988];

wordlength  = 16; % filter_coeff length
fractionalLength = 15;  % filter_coeff length assigned to fractional part


q = quantizer(wordlength, fractionalLength);

[min_range max_range] = range(q);  % Quatizer range -> -1.0 to 0.99969

% This will return two s compliment 16 bit filter coefficient
filter_coeff_bin = num2bin(q,filter_coeff);