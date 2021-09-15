
x_axis = linspace(-4, 4, 100);
y_axis = linspace(-4, 8, 100);
z = zeros(y_axis, x_axis);

for xx = (1:length(x_axis))
  for yy = (1:length(y_axis))
    x = x_axis(xx);
    y = y_axis(yy);
    a = x^2 - y;
    b = y - 4;
    z(yy, xx) = abs(a) + abs(b);
  endfor
endfor

imagesc(x_axis, y_axis, z);
colormap summer;
hold on;

% Draw path

raw = dlmread('path.csv', ',', 2, 0);

x = raw(:,1);
y = raw(:,2);

plot(x, y, 'LineWidth', 2, 'b')
